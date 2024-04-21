import java.util.*;  

class Solution {
    public List<Integer> getRow(int rowIndex) {
        
        int[][] pascal = new int[rowIndex+1][rowIndex+1];
        
        pascal[0][0] = 1;
        
        for(int i = 1; i < pascal.length; i++){
            for(int j = 0; j < pascal[i].length; j++){
                if(j == 0){
                    pascal[i][j] = 1;
                } else if(j == pascal[i].length-1){
                    pascal[i][j] = 1;
                } else{
                    pascal[i][j] = pascal[i-1][j-1]+pascal[i-1][j];
                }
            }
        }
        
        ArrayList<Integer> output = new ArrayList<>();
        for(int i = 0; i < pascal[rowIndex].length; i++){
            output.add(pascal[rowIndex][i]);
        }
        return output;
    }
}
