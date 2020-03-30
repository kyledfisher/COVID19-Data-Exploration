/* 
 * fast_reader.cpp
 * 
 * Read and parse lines in c++ for performance reasons.
 * readLines in R is somewhat slow.
 * 
 * TODO: actually make this faster.  Ovehead of calling SourceRCpp negates 
 *       performance benefit :(
 */


#include <iostream>
#include <fstream>
#include <list>
#include <Rcpp.h>


// [[Rcpp::export]]
std::vector<std::string> fastReadLines() {

    std::ifstream f {"/Users/zach/Code/COVID19-Data-Exploration/data/csse_covid_19_daily_reports"};
    std::string line;
    std::vector<std::string> linevec;
    
    if (!f) {
        std::cerr << "Cannot open file for reading" << std::endl;
    } 
    else {
        while(getline(f,line)) {
            linevec.push_back(line);
        }
    }
    
    return(linevec);
}