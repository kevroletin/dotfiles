#!/bin/sh

set -e

THIS_SCRIPT_DIR=$(dirname $0)
MAIL_SECRET=$(cat $THIS_SCRIPT_DIR/mail-secret.txt)
GET_AGENDA_SCRIPT=$THIS_SCRIPT_DIR/generate-agenda.el

/home/ubuntu/bin/org-pull

emacs --batch -l $GET_AGENDA_SCRIPT

sendemail -f "kevroletin@gmail.com" -u "Dayly agenda" -t "kevroletin@gmail.com" -s "smtp.gmail.com:587" -o tls=yes -xu "kevroletin@gmail.com" -xp $MAIL_SECRET << END_MESSAGE
$(cat '/tmp/agenda.html')
END_MESSAGE
