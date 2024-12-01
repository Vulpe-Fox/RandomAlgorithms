#include <vector>
#include <string>

using namespace std;

class Solution {
public:
    string convert(string s, int numRows) {
        if(numRows==1) return s;
        vector<string> zigzag(numRows);
        int i=0, row = 0;
        bool direction = true;
        while(true){
            if(direction){
                while(row<numRows && i<s.size()) zigzag[row++].push_back(s[i++]);
                row = numRows-2;
            }else{
                while(row>=0 && i<s.size()) zigzag[row--].push_back(s[i++]);
                row = 1;
            }
            direction = !direction;
            if(i>=s.size()) break;
        }
        string ans = "";
        for(auto str: zigzag) ans += str;
        return ans;
    }
};