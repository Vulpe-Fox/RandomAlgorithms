import java.util.*;  

class Solution {
    public int[] twoSum(int[] nums, int target) {
        HashSet<Integer> set = new HashSet<>();
        HashMap<Integer, Integer> map = new HashMap<>();
        
        int[] result = new int[2];
        
        for(int i = 0; i < nums.length; i++){
            if(set.contains(target - nums[i])){
                result[0] = map.get(target - nums[i]);
                result[1] = i;
                break;
            } else{
                set.add(nums[i]);
                map.put(nums[i], i);
            }
        }
        return result;
    }
}
