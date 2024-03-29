{ system ? builtins.currentSystem
, obelisk ? import ./.obelisk/impl {
    inherit system;
    iosSdkVersion = "13.2";
    config.android_sdk.accept_license = true;

    # You must accept the Android Software Development Kit License Agreement at
    # https://developer.android.com/studio/terms in order to build Android apps.
    # Uncomment and set this to `true` to indicate your acceptance:
    # config.android_sdk.accept_license = false;

    # In order to use Let's Encrypt for HTTPS deployments you must accept
    # their terms of service at https://letsencrypt.org/repository/.
    # Uncomment and set this to `true` to indicate your acceptance:
    # terms.security.acme.acceptTerms = false;
  }
}:
with obelisk;
project ./. ({ pkgs, hackGet,... }: {
  android.applicationId = "systems.obsidian.obelisk.examples.raf";
  android.displayName = "Obelisk RAF";
  ios.bundleIdentifier = "systems.obsidian.obelisk.examples.raf";
  ios.bundleName = "Obelisk RAF";

  packages = {
    psql = hackGet .dep/psql;
  };
})
