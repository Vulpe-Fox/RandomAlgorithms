#include <vector>
#include <string>
#include <unordered_map>
#include <algorithm>
#include <ranges>

using namespace std;

class Solution {
public:
    vector<vector<int>> fourSum(vector<int>& nums, int target) {
        ranges::sort(nums);

        vector<vector<int>> result;
        int n = static_cast<int>(nums.size());

        // First loop: fix nums[i]
        for (int i = 0; i < n; i++) {
            if (i > 0 && nums[i] == nums[i - 1]) {
                continue;
            }

            // Second loop: fix nums[j]
            for (int j = i + 1; j < n; j++) {
                if (j > i + 1 && nums[j] == nums[j - 1]) {
                    continue;
                }

                // Two-pointer approach for the remaining two numbers
                int left = j + 1;
                int right = n - 1;

                while (left < right) {
                    long long sum = static_cast<long long>(nums[i]) +
                                    nums[j] + nums[left] + nums[right];

                    if (sum < target) {
                        ++left;
                    } else if (sum > target) {
                        --right;
                    } else {
                        // Found a quadruplet
                        result.push_back({nums[i], nums[j], nums[left], nums[right]});

                        int oldLeft = nums[left];
                        while (left < right && nums[left] == oldLeft) {
                            ++left;
                        }

                        int oldRight = nums[right];
                        while (left < right && nums[right] == oldRight) {
                            --right;
                        }
                    }
                }
            }
        }
        return result;
    }
};