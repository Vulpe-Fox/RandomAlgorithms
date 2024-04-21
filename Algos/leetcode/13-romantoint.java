class Solution {
    
    int getValue(char r){
        switch(r){
            case 'I':
                return 1;
            case 'V':
                return 5;
            case 'X':
                return 10;
            case 'L':
                return 50;
            case 'C':
                return 100;
            case 'D':
                return 500;
            case 'M':
                return 1000;
            default:
                return 0;
        }
    }
    
    public int romanToInt(String s) {
        int result = 0;
        
        for(int i = 0; i < s.length(); i++){
            int s1 = getValue(s.charAt(i));
                
            if(i+1 < s.length()){
                int s2 = getValue(s.charAt(i+1));
                
                if(s1 >= s2){
                    result += s1;
                } else{
                    result += s2 - s1;
                    i++;
                }
            } else{
                result += s1;
            }
        }
        return result;
    }
}
