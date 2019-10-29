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

  testScript = ''
    $server->start();
    $server->waitForUnit("obiwan.socket");
    
    $server->succeed("tftp -m binary localhost -c get hello.world");
    $server->succeed("[ -f hello.world ]");
    $server->succeed("[ \"\$(cat hello.world)\" = 'hello world' ]");
  '';
}
