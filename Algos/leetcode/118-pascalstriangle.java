import java.util.*;  

class Solution {
    public List<List<Integer>> generate(int numRows) {
        List<List<Integer>> list = new ArrayList<List<Integer>>();
        
        int rowsLeft = numRows;
        
        
        
        while(rowsLeft > 0){
            List<Integer> newList = new ArrayList<Integer>();
            if(rowsLeft == numRows){
                newList.add(1);
                list.add(newList);
            } else{
                List<Integer> prevList = list.get(list.size()-1);
                newList.add(1);
                for(int i = 1; i < numRows - rowsLeft; i++){
                    newList.add(prevList.get(i-1) + prevList.get(i));
                }
                newList.add(1);
                list.add(newList);
            }
            rowsLeft--;
        }
        
        return list;
    }
}
