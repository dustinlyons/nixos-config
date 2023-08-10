{ config, pkgs, ... }:

let
  home           = builtins.getEnv "HOME";
  xdg_configHome = "${home}/.config";
  xdg_dataHome   = "${home}/.local/share";
  xdg_stateHome  = "${home}/.local/state"; in
{

  # Raycast script so that "Run Emacs" is available and uses Emacs daemon
  "${xdg_dataHome}/bin/emacsclient" = {
    executable = true;
    text = ''
      #!/bin/zsh
      #
      # Required parameters:
      # @raycast.schemaVersion 1
      # @raycast.title Run Emacs
      # @raycast.mode silent
      #
      # Optional parameters:
      # @raycast.packageName Emacs
      # @raycast.icon ${xdg_dataHome}/img/icons/Emacs.icns
      # @raycast.iconDark ${xdg_dataHome}/img/icons/Emacs.icns

      if [[ $1 = "-t" ]]; then
        # Terminal mode
        ${pkgs.emacs}/bin/emacsclient -t $@
      else
        # GUI mode
        ${pkgs.emacs}/bin/emacsclient -c -n $@
      fi
    '';
  };

  # Script to import Drafts into Emacs org-roam
  "${xdg_dataHome}/bin/import-drafts" = {
    executable = true;
    text = ''
      #!/bin/sh

      for f in ${xdg_stateHome}/drafts/*
      do
        if [[ ! "$f" =~ "done" ]]; then
          echo "Importing $f"
          filename="$(head -c 10 $f)"
          output="${xdg_dataHome}/org-roam/daily/$filename.org"
          echo '\n' >> "$output"
          tail -n +3 $f >> "$output"
          mv $f done
        fi
      done
    '';
  };

  "${xdg_dataHome}/Library/Application Support/Syncthing/cert.pem".text = "Temp";
  "${xdg_dataHome}/Library/Application Support/Syncthing/key.pem".text = "Temp";
  "${xdg_dataHome}/Library/Application Support/Syncthing/config.xml" = {
    text = ''
      <configuration version="37">
            <folder id="ukrub-quh7k" label="XDG Share" path="${xdg_dataHome}/.local/share" type="sendreceive" rescanIntervalS="3600" fsWatcherEnabled="true" fsWatcherDelayS="10" ignorePerms="false" autoNormalize="true">
              <filesystemType>basic</filesystemType>
              <device id="P2FYLQW-PKDFJGZ-EUGI2T7-OW4AH4I-KI462HD-U2VL3X3-GN55PP2-VNRE5AH" introducedBy="">
                  <encryptionPassword></encryptionPassword>
              </device>
              <device id="WW5O366-THBBBA3-HKQAYCP-EWADS4I-4KDDC5Z-3JCO42M-RLBZ3DY-NM7PEQA" introducedBy="">
                  <encryptionPassword></encryptionPassword>
              </device>
              <minDiskFree unit="%">1</minDiskFree>
              <versioning>
                  <cleanupIntervalS>3600</cleanupIntervalS>
                  <fsPath></fsPath>
                  <fsType>basic</fsType>
              </versioning>
              <copiers>0</copiers>
              <pullerMaxPendingKiB>0</pullerMaxPendingKiB>
              <hashers>0</hashers>
              <order>random</order>
              <ignoreDelete>false</ignoreDelete>
              <scanProgressIntervalS>0</scanProgressIntervalS>
              <pullerPauseS>0</pullerPauseS>
              <maxConflicts>10</maxConflicts>
              <disableSparseFiles>false</disableSparseFiles>
              <disableTempIndexes>false</disableTempIndexes>
              <paused>false</paused>
              <weakHashThresholdPct>25</weakHashThresholdPct>
              <markerName>.stfolder</markerName>
              <copyOwnershipFromParent>false</copyOwnershipFromParent>
              <modTimeWindowS>0</modTimeWindowS>
              <maxConcurrentWrites>2</maxConcurrentWrites>
              <disableFsync>false</disableFsync>
              <blockPullOrder>standard</blockPullOrder>
              <copyRangeMethod>standard</copyRangeMethod>
              <caseSensitiveFS>false</caseSensitiveFS>
              <junctionsAsDirs>false</junctionsAsDirs>
              <syncOwnership>false</syncOwnership>
              <sendOwnership>false</sendOwnership>
              <syncXattrs>false</syncXattrs>
              <sendXattrs>false</sendXattrs>
              <xattrFilter>
                  <maxSingleEntrySize>1024</maxSingleEntrySize>
                  <maxTotalSize>4096</maxTotalSize>
              </xattrFilter>
          </folder>
          <folder id="ukrub-quh8k" label="XDG State" path="${xdg_dataHome}/.local/state" type="sendreceive" rescanIntervalS="3600" fsWatcherEnabled="true" fsWatcherDelayS="10" ignorePerms="false" autoNormalize="true">
              <filesystemType>basic</filesystemType>
              <device id="P2FYLQW-PKDFJGZ-EUGI2T7-OW4AH4I-KI462HD-U2VL3X3-GN55PP2-VNRE5AH" introducedBy="">
                  <encryptionPassword></encryptionPassword>
              </device>
              <device id="WW5O366-THBBBA3-HKQAYCP-EWADS4I-4KDDC5Z-3JCO42M-RLBZ3DY-NM7PEQA" introducedBy="">
                  <encryptionPassword></encryptionPassword>
              </device>
              <minDiskFree unit="%">1</minDiskFree>
              <versioning>
                  <cleanupIntervalS>3600</cleanupIntervalS>
                  <fsPath></fsPath>
                  <fsType>basic</fsType>
              </versioning>
              <copiers>0</copiers>
              <pullerMaxPendingKiB>0</pullerMaxPendingKiB>
              <hashers>0</hashers>
              <order>random</order>
              <ignoreDelete>false</ignoreDelete>
              <scanProgressIntervalS>0</scanProgressIntervalS>
              <pullerPauseS>0</pullerPauseS>
              <maxConflicts>10</maxConflicts>
              <disableSparseFiles>false</disableSparseFiles>
              <disableTempIndexes>false</disableTempIndexes>
              <paused>false</paused>
              <weakHashThresholdPct>25</weakHashThresholdPct>
              <markerName>.stfolder</markerName>
              <copyOwnershipFromParent>false</copyOwnershipFromParent>
              <modTimeWindowS>0</modTimeWindowS>
              <maxConcurrentWrites>2</maxConcurrentWrites>
              <disableFsync>false</disableFsync>
              <blockPullOrder>standard</blockPullOrder>
              <copyRangeMethod>standard</copyRangeMethod>
              <caseSensitiveFS>false</caseSensitiveFS>
              <junctionsAsDirs>false</junctionsAsDirs>
              <syncOwnership>false</syncOwnership>
              <sendOwnership>false</sendOwnership>
              <syncXattrs>false</syncXattrs>
              <sendXattrs>false</sendXattrs>
              <xattrFilter>
                  <maxSingleEntrySize>1024</maxSingleEntrySize>
                  <maxTotalSize>4096</maxTotalSize>
              </xattrFilter>
          </folder>
          <device id="J4IKNWM-4G34UUN-OVVTPYD-OIBUNKK-D6BUNVD-UO5GNTW-FP2E5OK-LFF2DQD" name="Desktop" compression="metadata" introducer="false" skipIntroductionRemovals="false" introducedBy="">
              <address>dynamic</address>
              <paused>false</paused>
              <autoAcceptFolders>false</autoAcceptFolders>
              <maxSendKbps>0</maxSendKbps>
              <maxRecvKbps>0</maxRecvKbps>
              <maxRequestKiB>0</maxRequestKiB>
              <untrusted>false</untrusted>
              <remoteGUIPort>0</remoteGUIPort>
          </device>
          <device id="P2FYLQW-PKDFJGZ-EUGI2T7-OW4AH4I-KI462HD-U2VL3X3-GN55PP2-VNRE5AH" name="Macbook Pro" compression="metadata" introducer="false" skipIntroductionRemovals="false" introducedBy="">
              <address>dynamic</address>
              <paused>false</paused>
              <autoAcceptFolders>false</autoAcceptFolders>
              <maxSendKbps>0</maxSendKbps>
              <maxRecvKbps>0</maxRecvKbps>
              <maxRequestKiB>0</maxRequestKiB>
              <untrusted>false</untrusted>
              <remoteGUIPort>0</remoteGUIPort>
          </device>
          <device id="WW5O366-THBBBA3-HKQAYCP-EWADS4I-4KDDC5Z-3JCO42M-RLBZ3DY-NM7PEQA" name="Home Lab Server" compression="metadata" introducer="false" skipIntroductionRemovals="false" introducedBy="">
              <address>dynamic</address>
              <paused>false</paused>
              <autoAcceptFolders>false</autoAcceptFolders>
              <maxSendKbps>0</maxSendKbps>
              <maxRecvKbps>0</maxRecvKbps>
              <maxRequestKiB>0</maxRequestKiB>
              <untrusted>false</untrusted>
              <remoteGUIPort>0</remoteGUIPort>
          </device>
          <gui enabled="true" tls="false" debugging="false">
              <address>127.0.0.1:8384</address>
              <apikey>Rz2pTkXHUTpCvZA6CyVsfbrWDZYEb2Q2</apikey>
              <theme>default</theme>
          </gui>
          <ldap></ldap>
          <options>
              <listenAddress>default</listenAddress>
              <globalAnnounceServer>default</globalAnnounceServer>
              <globalAnnounceEnabled>true</globalAnnounceEnabled>
              <localAnnounceEnabled>true</localAnnounceEnabled>
              <localAnnouncePort>21027</localAnnouncePort>
              <localAnnounceMCAddr>[ff12::8384]:21027</localAnnounceMCAddr>
              <maxSendKbps>0</maxSendKbps>
              <maxRecvKbps>0</maxRecvKbps>
              <reconnectionIntervalS>60</reconnectionIntervalS>
              <relaysEnabled>true</relaysEnabled>
              <relayReconnectIntervalM>10</relayReconnectIntervalM>
              <startBrowser>true</startBrowser>
              <natEnabled>true</natEnabled>
              <natLeaseMinutes>60</natLeaseMinutes>
              <natRenewalMinutes>30</natRenewalMinutes>
              <natTimeoutSeconds>10</natTimeoutSeconds>
              <urAccepted>3</urAccepted>
              <urSeen>3</urSeen>
              <urUniqueID>cJd7qYdg</urUniqueID>
              <urURL>https://data.syncthing.net/newdata</urURL>
              <urPostInsecurely>false</urPostInsecurely>
              <urInitialDelayS>1800</urInitialDelayS>
              <autoUpgradeIntervalH>12</autoUpgradeIntervalH>
              <upgradeToPreReleases>false</upgradeToPreReleases>
              <keepTemporariesH>24</keepTemporariesH>
              <cacheIgnoredFiles>false</cacheIgnoredFiles>
              <progressUpdateIntervalS>5</progressUpdateIntervalS>
              <limitBandwidthInLan>false</limitBandwidthInLan>
              <minHomeDiskFree unit="%">1</minHomeDiskFree>
              <releasesURL>https://upgrades.syncthing.net/meta.json</releasesURL>
              <overwriteRemoteDeviceNamesOnConnect>false</overwriteRemoteDeviceNamesOnConnect>
              <tempIndexMinBlocks>10</tempIndexMinBlocks>
              <trafficClass>0</trafficClass>
              <setLowPriority>true</setLowPriority>
              <maxFolderConcurrency>0</maxFolderConcurrency>
              <crashReportingURL>https://crash.syncthing.net/newcrash</crashReportingURL>
              <crashReportingEnabled>true</crashReportingEnabled>
              <stunKeepaliveStartS>180</stunKeepaliveStartS>
              <stunKeepaliveMinS>20</stunKeepaliveMinS>
              <stunServer>default</stunServer>
              <databaseTuning>auto</databaseTuning>
              <maxConcurrentIncomingRequestKiB>0</maxConcurrentIncomingRequestKiB>
              <announceLANAddresses>true</announceLANAddresses>
              <sendFullIndexOnUpgrade>false</sendFullIndexOnUpgrade>
              <connectionLimitEnough>0</connectionLimitEnough>
              <connectionLimitMax>0</connectionLimitMax>
              <insecureAllowOldTLSVersions>false</insecureAllowOldTLSVersions>
          </options>
          <defaults>
              <folder id="" label="" path="~" type="sendreceive" rescanIntervalS="3600" fsWatcherEnabled="true" fsWatcherDelayS="10" ignorePerms="false" autoNormalize="true">
                  <filesystemType>basic</filesystemType>
                  <device id="P2FYLQW-PKDFJGZ-EUGI2T7-OW4AH4I-KI462HD-U2VL3X3-GN55PP2-VNRE5AH" introducedBy="">
                      <encryptionPassword></encryptionPassword>
                  </device>
                  <minDiskFree unit="%">1</minDiskFree>
                  <versioning>
                      <cleanupIntervalS>3600</cleanupIntervalS>
                      <fsPath></fsPath>
                      <fsType>basic</fsType>
                  </versioning>
                  <copiers>0</copiers>
                  <pullerMaxPendingKiB>0</pullerMaxPendingKiB>
                  <hashers>0</hashers>
                  <order>random</order>
                  <ignoreDelete>false</ignoreDelete>
                  <scanProgressIntervalS>0</scanProgressIntervalS>
                  <pullerPauseS>0</pullerPauseS>
                  <maxConflicts>10</maxConflicts>
                  <disableSparseFiles>false</disableSparseFiles>
                  <disableTempIndexes>false</disableTempIndexes>
                  <paused>false</paused>
                  <weakHashThresholdPct>25</weakHashThresholdPct>
                  <markerName>.stfolder</markerName>
                  <copyOwnershipFromParent>false</copyOwnershipFromParent>
                  <modTimeWindowS>0</modTimeWindowS>
                  <maxConcurrentWrites>2</maxConcurrentWrites>
                  <disableFsync>false</disableFsync>
                  <blockPullOrder>standard</blockPullOrder>
                  <copyRangeMethod>standard</copyRangeMethod>
                  <caseSensitiveFS>false</caseSensitiveFS>
                  <junctionsAsDirs>false</junctionsAsDirs>
                  <syncOwnership>false</syncOwnership>
                  <sendOwnership>false</sendOwnership>
                  <syncXattrs>false</syncXattrs>
                  <sendXattrs>false</sendXattrs>
                  <xattrFilter>
                      <maxSingleEntrySize>1024</maxSingleEntrySize>
                      <maxTotalSize>4096</maxTotalSize>
                  </xattrFilter>
              </folder>
              <device id="" compression="metadata" introducer="false" skipIntroductionRemovals="false" introducedBy="">
                  <address>dynamic</address>
                  <paused>false</paused>
                  <autoAcceptFolders>false</autoAcceptFolders>
                  <maxSendKbps>0</maxSendKbps>
                  <maxRecvKbps>0</maxRecvKbps>
                  <maxRequestKiB>0</maxRequestKiB>
                  <untrusted>false</untrusted>
                  <remoteGUIPort>0</remoteGUIPort>
              </device>
              <ignores></ignores>
          </defaults>
      </configuration>
  '';
  };
}
