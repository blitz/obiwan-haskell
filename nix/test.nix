{ nixosTest, runCommand, obiwan, obiwanModule }:

nixosTest {

  nodes = {
    server = { config, pkgs, ... }: {
      imports = [ obiwanModule ];
      services.obiwan = {
        enable = true;
        package = obiwan;

        rootDir = runCommand "tftproot" {} ''
          mkdir "$out"
          echo "hello world" > $out/hello.world
        '';
      };

      environment.systemPackages = [ pkgs.tftp-hpa ];
    };
  };

  # TODO
  testScript = ''
    $server->start();
    $server->waitForUnit("network-online.target");

    print($server->execute("systemctl status obiwan.socket"));
    print($server->execute("systemctl status obiwan.service"));
    
    $server->succeed("tftp -m binary localhost -c get /test");
  '';
}
