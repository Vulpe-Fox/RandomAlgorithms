class Solution {
    public int maxDepth(TreeNode root) {
        if(root != null){
            int leftDepth = 1 + maxDepth(root.left);
            int rightDepth = 1 + maxDepth(root.right);
            
            if(leftDepth > rightDepth){
                return leftDepth;
            } else{
                return rightDepth;
            }
        } else{
            return 0;
        }
        
    }
}