#bugs
1. Filter by Position: 这项不能为空，必须要有选择项。哪怕通过reset重置过后，依旧检索上一次的输入值。由于用的数据库中没有all这个选项，只有检索栏里面的，而且data frame是直接套用数据库中的值所以不能为空，以后有时间改。
2. 本来想加一个child table在output result里面，但是由于pitching和batting table的columns对不上，并且他们两个table的数据值是完全不一样的，投手和打者的数据。所以data frame也对不上没法直接导入，要是想创建child table还得进行更深层次的data cleaning，以后有时间改。
3. Interactive map没办法通过直接点击球队图标直接进行搜索，是因为1的原因，必须把filter by position的值输入进去才可以点击球队图标进行query。
4. UI布局太丑了，这个有心无力。
5. R有maria db的package，但是我用的这个数据库没法restore，只能用这个数据库的R package进行索引，没法insert新的value，以后有时间改。
