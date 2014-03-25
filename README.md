```
      __ __             ___      
     /\ \\ \           /\_ \     
 _ __\ \ \\ \    __  __\//\ \    
/\`'__\ \ \\ \_ /\ \/\ \ \ \ \   
\ \ \/ \ \__ ,__\ \ \_/ | \_\ \_ 
 \ \_\  \/_/\_\_/\ \___/  /\____\
  \/_/     \/_/   \/__/   \/____/
                                 
```

## Collection of R-scripts and functions for vaclab

## Install

The following R-packages are needed:

       rJava
       xlsxjars
       xlsx    
       knitr   
       methods 
       bitops  
       RJSONIO 
       RCurl   
       R4CouchDB

To install ``` bitops ``` and ```RCurl``` one need to install

        zypper in gcc-c++ R-base-devel libcurl-devel 

A java installation is needed for xlsx. It turned out to be a good idea to

        zypper in java-1.7.0-openjdk java-1.7.0-openjdk-devel

followed by

        R CMD javareconf

in the directory ```/usr/lib/R/library```. Afterwards (at least here) I could  install the xlsx stuff. 

### r4vl

       cd /usr/local/lib 
       git clone github_or_gitlab/r4vl.git 

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
