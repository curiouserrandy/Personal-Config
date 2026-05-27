#!/bin/bash
rclone bisync gDrive:Personal /home/randy/Personal --check-access --fast-list --track-renames --log-file /home/randy/Logs/gdrive-personal.log
rclone bisync gDrive:Projects /home/randy/Projects --check-access --fast-list --track-renames --log-file /home/randy/Logs/gdrive-projects.log
