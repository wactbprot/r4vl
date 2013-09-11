<pre>
      __ __             ___      
     /\ \\ \           /\_ \     
 _ __\ \ \\ \    __  __\//\ \    
/\`'__\ \ \\ \_ /\ \/\ \ \ \ \   
\ \ \/ \ \__ ,__\ \ \_/ | \_\ \_ 
 \ \_\  \/_/\_\_/\ \___/  /\____\
  \/_/     \/_/   \/__/   \/____/
                                 
</pre>

## Collection of R-scripts and functions for vaclab

## Install

The following R-packages are needed:

       rJava
       xlsxjars
       xlsx    
       knitr   
       reshape2
       ggplot2 
       methods 
       bitops  
       RJSONIO 
       RCurl   
       R4CouchDB


### java needed for xlsx

It turned out to be a good idea to

        zypper in java-1.7.0-openjdk-devel

and 

        zypper in java-devel

followed by

        R CMD javareconf

Afterwards (at least here) I could  install the xlsx stuff. 

### r4vl



       cd /usr/local/lib 
       git clone http://wellknowngitblitserver:8080/gitblit/git/r4vl.git 

Thats it.

## Task 

The Tasks look like this:


      {
        "Action": "/usr/bin/Rscript",
        "Comment": "Berechnet die p_cal & u(p_cal)",
        "TaskName": "CE3_calculate",
        "Value": [
            "/usr/local/lib/r4vl/map.R",
            "scripts/analyse_CE3_YAMP.R"
        ]
       }
