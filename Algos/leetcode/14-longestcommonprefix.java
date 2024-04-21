import java.util.*;

class Solution {
    public String longestCommonPrefix(String[] strs) {
        int arrSize = strs.length;
        
        if(strs.length == 0){
            return "";
        }
        
        Arrays.sort(strs);
        
        int end = Math.min(strs[0].length(), strs[arrSize-1].length());
        
        int prefixLen = 0;
        while(prefixLen < end && strs[0].charAt(prefixLen) == strs[arrSize - 1].charAt(prefixLen)){
            prefixLen++;
        }
        
        String prefix = strs[0].substring(0,prefixLen);
        return prefix;
    }
}
