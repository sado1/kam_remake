## About

Master Server performs a number of important functions:
 - Provides "Announcement" for the multiplayer lobby
 - Maintains a list of public multiplayer servers
 - Handles crashreports and redirects them to specified emails
 - Collects and provides statistics about servers and players over time

## API (incomplete)

### /index.php
Returns stub message

### /announcements.php
Returns master server announcement message

### /crashupload.php
Receiver of the crashreports

### /kamclub.php
Returns table of active game servers  
Required parameters:
 - type*
   - list returns a list
   - graph returns a chart
 
Examples:
 - /kamclub.php?type=list
 - /kamclub.php?type=graph

..wip..

### /serverquery.php?rev=r16176

Required parameters:
 - rev*
   - r16176
 - format
   - ajaxupdate ?
   - refresh includes JS refresh code?
   - kamclub table with Rus column names
   - kamclubeng table with Rus column names
   - table table

Examples:
 - /serverquery.php?rev=r16176
 - /serverquery.php?rev=r16176&format=table

### /serverstats.php

Returns `There are 0 servers running and 0 players online`

### /serverstatsandtime.php

Returns
```
There are 0 servers running and 0 players online
Total player-time:
0 years, 7 days, 7 hours
```

### /servertime.php

Returns `0 years, 7 days, 7 hours`


### /statistics.php

Allows to configure and see chart
