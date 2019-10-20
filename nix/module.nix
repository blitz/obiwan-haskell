{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.obiwan;
  tftpPort = 69;
in {
  options.services.obiwan = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Whether to enable te Obiwan TFTP server, a simple Trivial File
        Transfer Protocol server usually used for PXE booting. The service
        will be run as a socket-activated systemd service.
      '';
    };

    rootDir = mkOption {
      default = "/var/spool/tftp";
      description = ''
        The directory that will be served via TFTP.
      '';
    };

    # TODO This should use systemd magic and a temporary user.
    user = mkOption {
      type = types.str;
      default = "nobody";
      description = "User account under which obiwan runs.";
    };

    package = mkOption {
      type = types.package;
      default = pkgs.obiwan;
      description = "Which Obiwan derivation to use";
    };

    openFirewall = mkOption {
      type = types.bool;
      default = true;
      description = "Open ports in the firewall for the TFTP Server";
    };

  };


  # See this blog post for an example of how to use systemd sockets:
  # http://0pointer.de/blog/projects/inetd.html

  # TODO
  # - dynamic user support
  # - sandboxing
  config = mkIf cfg.enable {

    networking.firewall = mkIf cfg.openFirewall {
      allowedUDPPorts = [ tftpPort ];
    };

    systemd.sockets.obiwan = {
      description = "Obiwan TFTP Server Socket";
      wantedBy = ["sockets.target"];

      socketConfig = {
        ListenDatagram = tftpPort;
        Accept = false;
      };
    };

    systemd.services.obiwan = {
      description = "Obiwan TFTP Per-Connection Server";

      serviceConfig = {
        User = cfg.user;
        ExecStart = "${cfg.package}/bin/obiwan --systemd";

        WorkDirectory = cfg.rootDir;
        
        # TODO Would be nice to eventually support this.
        #DynamicUser = true;
      };
    };
  };
}
