{ pkgs, ... }:

{
  environment.systemPackages = [
    pkgs.arduino
    pkgs.arduino-cli
  ];

  # Arduino Giga R1 UDEV Rule
  # https://github.com/arduino/ArduinoCore-mbed/blob/main/post_install.sh
  services.udev.extraRules = ''
    SUBSYSTEMS=="usb", ATTRS{idVendor}=="2e8a", MODE:="0666"
    SUBSYSTEMS=="usb", ATTRS{idVendor}=="2341", MODE:="0666"
    SUBSYSTEMS=="usb", ATTRS{idVendor}=="1fc9", MODE:="0666"
    SUBSYSTEMS=="usb", ATTRS{idVendor}=="0525", MODE:="0666"
  '';

}
