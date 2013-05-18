(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

open Stringext
open Threadext
open Pervasiveext
open Fun
open Xen_api
open Client.Client
open Cohttp

module IO = struct
	type 'a t = 'a
	let ( >>= ) a f = f a
	let (>>) m n = m >>= fun _ -> n

	let return a = a

	let iter = List.iter

	type ic = in_channel
	type oc = out_channel

	let read_line ic =
		try
			let line = input_line ic in
			let last = String.length line - 1 in
			let line = if line.[last] = '\r' then String.sub line 0 last else line in
			Some line
		with _ -> None

	let read_into_exactly ic buf ofs len =
		try
			really_input ic buf ofs len; true
		with _ -> false
	let read_exactly ic len =
		let buf = String.create len in
		read_into_exactly ic buf 0 len >>= function
		| true -> return (Some buf)
		| false -> return None

	let read ic n =
		let buf = String.make n '\000' in
		let actually_read = input ic buf 0 n in
		if actually_read = n
		then buf
		else String.sub buf 0 actually_read

	let write oc x = 
		output_string oc x; flush oc

	let open_connection uri =
		let addr = match Uri.host uri with
			| Some host -> host
			| None -> failwith "no addr" in
		let ssl = match Uri.scheme uri with
			| Some "http" -> false
			| Some "https" -> true
			| Some x -> failwith ("unsupported_scheme " ^  x)
			| None -> failwith "Unsupported_scheme" in
		let port = match Uri.port uri with
			| Some x -> x
			| None -> if ssl then 443 else 80 in
		let sockaddr = Unix.ADDR_INET(Unix.inet_addr_of_string addr, port) in
		let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
		let () = Unix.connect fd sockaddr in
		Unix.setsockopt fd Unix.TCP_NODELAY true;
		let ic = Unix.in_channel_of_descr fd in
		let oc = Unix.out_channel_of_descr fd in
		Ok (ic, oc)

	let close (ic, oc) =
		close_in ic;
		close_out oc
	let sleep = Thread.delay
	let gettimeofday = Unix.gettimeofday
end

module M = Make(IO)

let do_it uri string =
	let uri = Uri.of_string uri in
	let connection = M.make uri in
	let result = M.rpc connection string in
	match result with
		| Ok x -> x
		| Error e -> raise e

let make ?(timeout=30.) uri call =
	let string = Xmlrpc.string_of_call call in
	let result = do_it uri string in
	Xmlrpc.response_of_string result

let ref_null = API.Ref.of_string "OpaqueRef:NULL"

(*

let parse_source x = match List.filter (fun x -> x <> "") (String.split ':' x) with
	| [ "phy"; path ] -> Some (Local path)
	| [ "sm"; path ] -> Some (VDI path)
	| [ "file"; path ] ->
		Printf.fprintf stderr "I don't understand 'file' disk paths. Please use 'phy'.\n";
		exit 2
	| [] -> None (* empty *)
	| _ ->
		Printf.fprintf stderr "I don't understand '%s'. Please use 'phy:path,...\n" x;
		exit 2
*)

let parse_pci (x, idx) =
	match String.split ',' x with
	| bdf :: options ->
		let hex x = int_of_string ("0x" ^ x) in
		let parse_dev_fn x = match String.split '.' x with
			| [ dev; fn ] -> hex dev, hex fn
			| _ ->
				Printf.fprintf stderr "Failed to parse BDF: %s. It should be '[DDDD:]BB:VV.F'\n" bdf;
				exit 2 in
		let domain, bus, dev, fn = match String.split ':' bdf with
			| [ domain; bus; dev_dot_fn ] ->
				let dev, fn = parse_dev_fn dev_dot_fn in
				hex domain, hex bus, dev, fn
			| [ bus; dev_dot_fn ] ->
				let dev, fn = parse_dev_fn dev_dot_fn in
				0, hex bus, dev, fn
			| _ ->
				Printf.fprintf stderr "Failed to parse BDF: %s. It should be '[DDDD:]BB:VV.F'\n" bdf;
				exit 2 in
	(*
		let options = List.map (fun x -> match String.split ~limit:2 '=' x with
			| [k; v] -> k, v
			| _ ->
				Printf.fprintf stderr "Failed to parse PCI option: %s. It should be key=value.\n" x;
				exit 2
		) options in
		let bool_opt k opts =
			if List.mem_assoc k opts then Some (List.assoc k opts = "1") else None in
		let open Xn_cfg_types in
		let msitranslate = bool_opt _msitranslate options in
		let power_mgmt = bool_opt _power_mgmt options in
	*)
		Printf.sprintf "%d/%04d:%02d:%02d.%01d" idx domain bus dev fn
	| _ ->
		Printf.fprintf stderr "Failed to parse PCI '%s'. It should be '[DDDD:]BB:VV.F[,option1[,option2]]'." x;
		exit 2

(*
let parse_disk vm_id x = match String.split ',' x with
	| [ source; device_number; rw ] ->
		let ty, device_number, device_number' = match String.split ':' device_number with
			| [ x ] -> Vbd.Disk, x, Device_number.of_string false x
			| [ x; "cdrom" ] -> Vbd.CDROM, x, Device_number.of_string false x
			| _ ->
				Printf.fprintf stderr "Failed to understand disk name '%s'. It should be 'xvda' or 'hda:cdrom'\n" device_number;
				exit 2 in
		let mode = match String.lowercase rw with
			| "r" -> Vbd.ReadOnly
			| "w" -> Vbd.ReadWrite
			| x ->
				Printf.fprintf stderr "Failed to understand disk mode '%s'. It should be 'r' or 'w'\n" x;
				exit 2 in
		let backend = parse_source source in
		{
			Vbd.id = vm_id, device_number;
			position = Some device_number';
			mode = mode;
			backend = backend;
			ty = ty;
			unpluggable = true;
			extra_backend_keys = [];
			extra_private_keys = [];
			qos = None;
		}
	| _ ->
		Printf.fprintf stderr "I don't understand '%s'. Please use 'phy:path,xvda,w'\n" x;
		exit 2
*)

let parse_vif x =
	let open Xn_cfg_types in
	let xs = List.filter (fun x -> x <> "") (List.map (String.strip String.isspace) (String.split ',' x)) in
	List.map (fun x -> match String.split ~limit:2 '=' x with
		| [ k; v ] -> k, v
		| _ ->
			Printf.fprintf stderr "I don't understand '%s'. Please use 'mac=xx:xx:xx:xx:xx:xx,bridge=xenbrX'.\n" x;
			exit 2
	) xs

let add filename =
	Unixext.with_input_channel filename
		(fun ic ->
			let lexbuf = Lexing.from_channel ic in
			let config = Xn_cfg_parser.file Xn_cfg_lexer.token lexbuf in
			let open Xn_cfg_types in
			let mem x = List.mem_assoc x config in
			let find x = List.assoc x config in
			let any xs = List.fold_left (||) false (List.map mem xs) in
			let rpc = make "http://10.80.228.25" in
			let session_id = Session.login_with_password rpc "root" "xenroot" "1.0" in
			finally (fun () ->
				print_endline "Logged in to xapi.";
				print_string "Creating VM... ";
				let name_label = if mem _name then find _name |> string else filename in
				let memory =
					let mib = if mem _memory then find _memory |> int |> Int64.of_int else 64L in
					let bytes = Int64.mul 1024L (Int64.mul 1024L mib) in
					bytes
				in
				let vcpus = if mem _vcpus then find _vcpus |> int |> Int64.of_int else 1L in

				let pv =
					false
					|| (mem _builder && (find _builder |> string = "linux"))
					|| (not(mem _builder) && (any [ _bootloader; _kernel ])) in
				let pV_bootloader = if mem _bootloader then find _bootloader |> string else "" in
				let pV_kernel = if mem _kernel then find _kernel |> string else "" in
				let pV_ramdisk = if mem _ramdisk then find _ramdisk |> string else "" in
				let pV_args = if mem _root then find _root |> string else "" in
				if pv && pV_bootloader = "" && pV_kernel = "" then begin
					List.iter (Printf.fprintf stderr "%s\n") [
						"I couldn't determine how to start this VM.";
						Printf.sprintf "A PV guest needs either %s or %s and %s" _bootloader _kernel _ramdisk
					];
					exit 1
				end;
				let hVM_boot_policy = if not pv then "BIOS order" else "" in
				let hVM_boot_params = if not pv then ["order", if mem _boot then find _boot |> string else "cd"] else [] in
				let platform = [] in
				let pcis = if mem _pci then find _pci |> list string else [] in
				let pcis = List.combine pcis (Range.to_list (Range.make 0 (List.length pcis))) in
				let other_config = ["pci", String.concat ";" (List.map parse_pci pcis)] in

				let vm = VM.create ~rpc ~session_id ~name_label ~name_description:"" ~user_version:1L ~is_a_template:false ~affinity:ref_null
					~memory_target:0L ~memory_static_max:memory ~memory_dynamic_max:memory ~memory_dynamic_min:memory ~memory_static_min:memory
					~vCPUs_params:[] ~vCPUs_max:vcpus ~vCPUs_at_startup:vcpus
					~actions_after_shutdown:`destroy ~actions_after_reboot:`restart ~actions_after_crash:`preserve
					~pV_bootloader ~pV_kernel ~pV_ramdisk ~pV_args ~pV_bootloader_args:"" ~pV_legacy_args:""
					~hVM_boot_policy ~hVM_boot_params ~hVM_shadow_multiplier:1. ~platform ~pCI_bus:""
					~other_config ~recommendations:"" ~xenstore_data:[] ~ha_always_run:false ~ha_restart_priority:"" ~tags:[]
					~blocked_operations:[] ~protection_policy:ref_null ~is_snapshot_from_vmpp:false ~appliance:ref_null ~start_delay:0L
					~shutdown_delay:0L ~order:0L ~suspend_SR:ref_null ~version:0L in
				print_endline vm;

				let networks = Network.get_all ~rpc ~session_id in
				let bridges_with_networks = List.map (fun network ->
					Network.get_bridge ~rpc ~session_id ~self:network,
					network
				) networks in

				let vifs = if mem _vif then find _vif |> list string else [] in
				let rec create_vifs idx = function
					| [] -> ()
					| vif :: vifs ->
						print_string "Creating VIF... ";
						let device = string_of_int idx in
						let vif = parse_vif vif in
						let network =
							if List.mem_assoc _bridge vif then
								let bridge = List.assoc _bridge vif in
								if List.mem_assoc bridge bridges_with_networks then
									List.assoc bridge bridges_with_networks
								else begin
									Printf.fprintf stderr "Cannot create VIF: no network for bridge %s" bridge;
									exit 1
								end
							else begin
								Printf.fprintf stderr "Cannot create VIF: no bridge specified";
								exit 1
							end
						in
						let mac = if List.mem_assoc _mac vif then List.assoc _mac vif else "" in
						let vif = VIF.create ~rpc ~session_id ~device ~network ~vM:vm ~mAC:mac ~mTU:1500L ~other_config:[] ~qos_algorithm_type:""
							~qos_algorithm_params:[] ~locking_mode:`unlocked ~ipv4_allowed:[] ~ipv6_allowed:[] in
						print_endline vif;
						create_vifs (idx + 1) vifs
				in
				create_vifs 0 vifs;

			) (fun () -> Session.logout rpc session_id);
			print_endline "Logged out.";
			(*
			let pv =
				false
				|| (mem _builder && (find _builder |> string = "linux"))
				|| (not(mem _builder) && (any [ _bootloader; _kernel ])) in
			let open Vm in
			let builder_info = match pv with
				| true -> PV {
					framebuffer = false;
					framebuffer_ip = Some "0.0.0.0";
					vncterm = true;
					vncterm_ip = Some "0.0.0.0";
					boot =
						if mem _bootloader then Indirect {
							bootloader = find _bootloader |> string;
							extra_args = "";
							legacy_args = "";
							bootloader_args = "";
							devices = [];
						} else if mem _kernel then Direct {
							kernel = find _kernel |> string;
							cmdline = if mem _root then find _root |> string else "";
							ramdisk = if mem _ramdisk then Some (find _ramdisk |> string) else None;
						} else begin
							List.iter (Printf.fprintf stderr "%s\n") [
								"I couldn't determine how to start this VM.";
								Printf.sprintf "A PV guest needs either %s or %s and %s" _bootloader _kernel _ramdisk
							];
							exit 1
						end;
					}
				| false -> HVM {
					hap = true;
					shadow_multiplier = 1.;
					timeoffset = "";
					video_mib = 4;
					video = Cirrus;
					acpi = true;
					serial = None;
					keymap = None;
					vnc_ip = Some "0.0.0.0";
					pci_emulations = [];
					pci_passthrough = false;
					boot_order = if mem _boot then find _boot |> string else "cd";
					qemu_disk_cmdline = false;
					qemu_stubdom = false;
				} in
			let uuid = if mem _uuid then find _uuid |> string else Uuid.string_of_uuid (Uuid.make_uuid ()) in
			let name = if mem _name then find _name |> string else uuid in
			let mib = if mem _memory then find _memory |> int |> Int64.of_int else 64L in
			let bytes = Int64.mul 1024L (Int64.mul 1024L mib) in
			let vcpus = if mem _vcpus then find _vcpus |> int else 1 in
			let pci_msitranslate = if mem _vm_pci_msitranslate then find _vm_pci_msitranslate |> bool else true in
			let pci_power_mgmt = if mem _vm_pci_power_mgmt then find _vm_pci_power_mgmt |> bool else false in
			let vm = {
				id = uuid;
				name = name;
				ssidref = 0l;
				xsdata = [];
				platformdata = [ (* HVM defaults *)
					"nx", "false";
					"acpi", "true";
					"apic", "true";
					"pae", "true";
					"viridian", "true";
				];
				bios_strings = [];
				ty = builder_info;
				suppress_spurious_page_faults = false;
				machine_address_size = None;
				memory_static_max = bytes;
				memory_dynamic_max = bytes;
				memory_dynamic_min = bytes;
				vcpu_max = vcpus;
				vcpus = vcpus;
				scheduler_params = { priority = None; affinity = [] };
				on_crash = [ Vm.Shutdown ];
				on_shutdown = [ Vm.Shutdown ];
				on_reboot = [ Vm.Start ];
				pci_msitranslate = pci_msitranslate;
				pci_power_mgmt = pci_power_mgmt;
			} in
			let (id: Vm.id) = Client.VM.add dbg vm in
			let disks = if mem _disk then find _disk |> list string else [] in
			let one x = x |> parse_disk id |> Client.VBD.add dbg in
			let (_: Vbd.id list) = List.map one disks in
			let vifs = if mem _vif then find _vif |> list string else [] in
			let vifs = List.combine vifs (Range.to_list (Range.make 0 (List.length vifs))) in
			let one x = x |> parse_vif id |> Client.VIF.add dbg in
			let (_: Vif.id list) = List.map one vifs in
			let pcis = if mem _pci then find _pci |> list string else [] in
			let pcis = List.combine pcis (Range.to_list (Range.make 0 (List.length pcis))) in
			let one x = x |> parse_pci id |> Client.PCI.add dbg in
			let (_: Pci.id list) = List.map one pcis in
			Printf.printf "%s\n" id
			*)
		)

let usage () =
	Printf.fprintf stderr "%s <command> [args] - send commands to the xenops daemon\n" Sys.argv.(0);
	Printf.fprintf stderr "%s add <config> - add a VM from <config>\n" Sys.argv.(0);
	()

let _ =
	let args = Sys.argv |> Array.to_list |> List.tl in
	let verbose = List.mem "-v" args in
	let args = List.filter (fun x -> x <> "-v") args in
	begin match args with
		| [ "help" ] | [] ->
			usage ();
			exit 0
		| [ "add"; filename ] ->
			add filename
		| cmd :: _ ->
			Printf.fprintf stderr "Unrecognised command: %s\n" cmd;
			usage ();
			exit 1
	end

