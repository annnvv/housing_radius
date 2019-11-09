<b>Hypothesis</b>: Average housing size (in square feet) increases as housing is farther away from the center of the Washington, DC (proxy Lincoln Memorial) and price roughly the same (or increases slightly, but probably does not decrease).

<b>Lessons Learned</b>:
1. Don't try to scrape zipcode data from websites (like Zillow which don't allow you to do so). Should have first merged all of the zipcode shapefiles together and gotten a complete list that way.
2. Figure out how many zipcodes there are and what the zipcodes are before downloading housing data from each zip code (so when downloading data you get all of it, instead of just thinking you have all of it). 
3. One is able to manually download up to about 350 data points at a time from Redfin. Their terms of use do not allow web scraping their data (even if yout try to build in seemingly random pauses into your scraping loop, they know).
4. When searching for answers on StackOverflow, double check when the question was answered AND updated. Sent a lot of type trying to update a package when I should have been installing the dev version.
5. Make sure to read the documentation for installing dev versions (David Cooley had solid documentation about installing the dev version for mapdeck).


