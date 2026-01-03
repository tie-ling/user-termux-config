#!/bin/sh
# install termux, emacs gui from fdroid.

# choose a repo
termux-change-repo

# install git and openssh immediately
# too hard on the eyes to run commands on small screeen

pkg install git openssh openssh-sftp-server emacs termux-services w3m dictd

# now press C-d to exit and restart termux
# this is needed for sv service manager to work

# now enable ssh services
sv-enable ssh-agent
sv-enable sshd

# aquire wakelock
go to notification , termux, aquire wakelock

# fix pager, or else w3m will be used
pkg install less
update-alternatives --all

# scan public key as qr code and paste:
## qrencode -t ansiutf8 < .ssh/id_ed25519.pub

nano .ssh/authorized_keys

# see ip
ifconfig

## ssh -p 8022 user@192.168.1.85

# clone home
git clone https://github.com/tie-ling/user-termux-config

# link config
mv ~/user-termux-config/.git ~/
git reset --hard

# set new remote
git -C ~ remote rm origin
git -C ~ remote add origin git@github.com:tie-ling/user-termux-config

# clone user-Projects
ssh-add

git clone vps:~/Projects Projects

# fix emacs info manual not displaying
pkg install texinfo
find /data/data/com.termux/files/usr/share/info/ -type f -exec \
install-info --dir-file=/data/data/com.termux/files/usr/share/info/dir \
--info-file={} \;
