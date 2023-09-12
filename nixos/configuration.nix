# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:
let
  customNodePackages = pkgs.callPackage ./customNodePackages {  };
in
{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "nixos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "America/New_York";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Enable the GNOME Desktop Environment.
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome.enable = true;

  # Configure keymap in X11
  services.xserver = {
    layout = "us";
    xkbVariant = "";
  };

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound with pipewire.
  sound.enable = true;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.devbox = {
    isNormalUser = true;
    description = "Jack Schumann";
    extraGroups = [ "networkmanager" "wheel" ];
    packages = with pkgs; [
      # user apps
      spotify
      # usertools
      unzip imagemagick xclip
      # devtools
      xfce.xfce4-terminal git delta jq wget htop tokei gh clang clang-tools
      # rust
      cargo rustc rust-analyzer rustfmt clippy
      # browsers
      firefox
      # aws things
      # customNodePackages."@aws-amplify/cli" # doesnt seem to work :/
            customNodePackages."@tailwindcss/language-server"
      customNodePackages."prettier"
      # https://www.reddit.com/r/NixOS/comments/om1wdw/comment/i8nzbf1/
      # https://github.com/aws-amplify/amplify-cli/issues/10193 
      awscli2
      # langs
      python3
      # KFS
      tailscale #vpn
      # X11?
      steam-run
      patchelf
      libGL
      libxkbcommon
      wayland
      xorg.libX11
      xorg.libXcursor
      xorg.libXi
      xorg.libXrandr
    ];
  };

  # Enable automatic login for the user.
  services.xserver.displayManager.autoLogin.enable = true;
  services.xserver.displayManager.autoLogin.user = "devbox";


  networking.nameservers = ["8.8.8.8" "8.8.4.4"];
  programs.npm.enable = true;

  systemd.user.services.kfsvpn = {
    script = ''
    ${pkgs.tailscale}/bin/tailscale up --hostname bonked --accept-routes --shields-up
  '';
    wantedBy = [ "default.target" ];
  };

  # vpn for KFS
  services.tailscale.enable = true;

  # Workaround for GNOME autologin: https://github.com/NixOS/nixpkgs/issues/103746#issuecomment-945091229
  # default apps
  xdg.mime = {
    enable = true;
    # note this will fail silently if chromium-browser does not exist or is misspelled 
    defaultApplications = {
      "text/html" = "chromium-browser.desktop";
    };
  };
  systemd.services."getty@tty1".enable = false;
  systemd.services."autovt@tty1".enable = false;

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    chromium
    emacs-nox
    # emacs deps:
    ispell nixfmt silver-searcher
    node2nix
    nodejs
  ];


  environment.etc = {
    "personalconfig/.emacs" = {
      text = builtins.readFile (pkgs.fetchurl {
        url =
          "https://raw.githubusercontent.com/jackschu/emacsconfig/a849beff6e56f3151ad3c75f1e308154f2edd4a5/.emacs";
        sha256 = "sha256-m5TQxCd9scPU5dhpjimDj3d8FMZ0yEANHcTg7mBG6Kc=";
        # sha256 = pkgs.lib.fakeSha256;
      });
    };
  };

  # Alias
  # environment.interactiveShellInit = ''
  #   export PATH="$HOME/open_source/amplify-cli/packages/amplify-cli/bin:$PATH"
  # '';
  programs.bash.shellAliases = {
    hg = "git";
    
  };
  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?

}
