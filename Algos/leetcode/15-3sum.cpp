#include <vector>
#include <string>
#include <unordered_map>
#include <algorithm>

using namespace std;

class Solution {
public:
    vector<vector<int>> threeSum(vector<int>& nums) {
        sort(nums.begin() , nums.end());     
        if(nums.size() < 3){               
            return {};
        }
        if(nums[0] > 0){                    
            return {};
        }
        unordered_map<int , int> hash;
        for(int i = 0 ; i < nums.size() ; ++i){    
            hash[nums[i]] = i;
        }
        vector<vector<int>> answer;
        for(int i = 0 ; i < nums.size() - 2 ; ++i){     
            if(nums[i] > 0){     
                break;
            }
            for(int j = i + 1 ; j < nums.size() - 1 ; ++j){     
                int required = -1*(nums[i] + nums[j]);    
                if(hash.count(required) && hash.find(required)->second > j){ 
                    answer.push_back({nums[i] , nums[j] , required});
                }
                j = hash.find(nums[j])->second; 
            }
            i = hash.find(nums[i])->second;     
        }
        return answer;  
    }
};