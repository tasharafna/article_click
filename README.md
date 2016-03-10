# article_click

This repo is designed to organize and analyze user clicks of several million articles sent over email. The data itself 
is simulated - it was provided as part of a test exercise. The data is tidy though it is arranged inconveniently in 
different tables and in a log file. 

There are two files used in this repo:

- final_wrangle.py : Loads user data from a sqlite database. Uses regex to parse server log file (which represents user 
clickstreams). Combined all data into a single frame, adds a few features, then spits up a single file.

- final_graph.R : Loads the file result from final_wrangle.py and performs EDA and some ML on the dataset. Graphs
results.
