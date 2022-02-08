## Climbing the stairs solution
n = 2
 def climbStairs(self, n: int) -> int:
        
        if n == 1 or n == 2:
            return n
        
        prevPrev = 1
        prev = 2
        current = 0
        
        for step in range(3, n+1):
            
            current = prevPrev + prev
            
            prevPrev = prev
            
            prev = current
            
            
        return current

## Richest customer
 accounts = [[1,2,3],[3,2,1]]
    def maximumWealth(self, accounts: List[List[int]]) -> int:
        ans, curr = 0,0        
        for i in accounts:
            for j in i:
                curr += j
            ans = max(ans,curr)
            curr = 0
        
        return ans
      
      
      
## Shuffle problem
 nums = [2,5,1,3,4,7]
    
    def shuffle(self, nums: List[int], n: int) -> List[int]:
        
        result = []
        
        
        for i in range(n):
            result.append(nums[i])
            result.append(nums[n+i])
        
        return result
      
      
## Number of good pairs
  nums = [1,2,3,1,1,3]
    def numIdenticalPairs(self, nums: List[int]) -> int:
        dec ={}
        for i in range(len(nums)):
            if nums[i] in dec:
                dec[nums[i]].append(i)
            else:
                dec[nums[i]] = [i]
                
        res = 0
        for key, values in dec.items():
            if len(values) >= 2:
                n = len(values)
                res += n*(n-1)/2
                
        return int(res)

## How many numbers are smaller than the current number
def smallerNumbersThanCurrent(self, nums: List[int]) -> List[int]:
        res = []
        l = [0] * 101
        for num in nums:
            l[num] += 1
        for i in range(1, len(l)):
            l[i] += l[i - 1]
        for num in nums:
            if num == 0:
                res.append(0)
            else:
                res.append(l[num - 1])
        return res
                
