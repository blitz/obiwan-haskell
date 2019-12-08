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
          echo "hello binary" > $out/binary.hello
          echo "hello ascii" > $out/ascii.hello
        '';
      };

      environment.systemPackages = [ pkgs.tftp-hpa ];
    };
  };

  testScript = ''
    $server->start();
    $server->waitForUnit("obiwan.socket");
    
    $server->succeed("tftp -m binary localhost -c get binary.hello");
    $server->succeed("[ -f binary.hello ] && [ \"\$(cat binary.hello)\" = 'hello binary' ]");

    $server->succeed("tftp -m ascii localhost -c get ascii.hello");
    $server->succeed("[ -f ascii.hello ] && [ \"\$(cat ascii.hello)\" = 'hello ascii' ]");
  '';
}
