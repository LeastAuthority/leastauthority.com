#! /usr/bin/env python

requesters = [line.split(",")[1].strip() for line in open("emails.csv", 'rU') if line.split(",")[2].strip() == "s4" and line.split(",")[1].strip() is not ""]

already_invited = set([line.split(",")[1].strip() for line in open("invited-s4.csv",'rU')])

print already_invited
nyis4re_handle = open("not_yet_invited_s4_requester_emails.csv",'w')

req_set = set()
for requester in requesters:
    if (requester not in already_invited) and (requester not in req_set):
        req_set.add(requester)
        nyis4re_handle.write(requester + '\n') 

nyis4re_handle.close()
