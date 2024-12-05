/**
 * Definition for singly-linked list.
 * public class ListNode {
 *     int val;
 *     ListNode next;
 *     ListNode() {}
 *     ListNode(int val) { this.val = val; }
 *     ListNode(int val, ListNode next) { this.val = val; this.next = next; }
 * }
 */
class Solution {
    public ListNode removeNthFromEnd(ListNode head, int n) {
        ListNode spare = new ListNode(0);
        spare.next = head;
        
        ListNode first = spare;
        ListNode second = spare;
        
        // Move `first` n+1 steps ahead to create the necessary gap
        for (int i = 0; i <= n; i++) {
            first = first.next;
        }
        
        // Move both `first` and `second` until `first` reaches the end
        while (first != null) {
            first = first.next;
            second = second.next;
        }
        
        // Remove the nth node from the end
        second.next = second.next.next;
        
        return spare.next; 
    }
}