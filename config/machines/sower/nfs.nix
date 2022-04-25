{ ... }:

{
  services.nfs.server = {
    enable = true;
    exports = ''
      /nfs         192.168.0.0/24 (rw,sync,fsid=0,no_subtree_check)
      /nfs/nas     192.168.0.0/24 (rw,sync,fsid=0,no_subtree_check)
    '';
    # fixed rpc.statd port; for firewall
    lockdPort = 4001;
    mountdPort = 4002;
    statdPort = 4000;
    extraNfsdConfig = '''';
  };

  networking.firewall.allowedTCPPorts = [ 111  2049 4000 4001 4002 20048 ];
  networking.firewall.allowedUDPPorts = [ 111 2049 4000 4001  4002 20048 ];
}
