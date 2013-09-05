
## no longer needed:

<pre>
## rproc

Kind of howto couchapp showing the implementation
of _external_ R calls with a __show_ like url.

R-functions and R-scripts are intended stored in the database;

rproc.R gets them at the first time (and on demand
using the url- param _ctrl=reload_)


## config

* Put the following 4 lines in your _local.ini_

     [external]

      rproc= Rscript  path/to/thiscouchapp/_attachments/rproc.R

     [httpd_db_handlers]

     _rproc = {couch_httpd_external, handle_external_req, <<"rproc">>}

* restart couchdb


## usage

I like to use this in the following manner

* client initiates a  GET http://server:5984/db/_rproc/doc_id?script=scriptname
* rproc.R gets provides the document with the doc_id
* scriptname.R runs the analysis process on the data stored in the document
* rproc.R updates the document
* GET- Request is answered with the new revision of the document
</pre>
