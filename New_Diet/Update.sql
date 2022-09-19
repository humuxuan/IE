/*export the file*/
select * from food.new_nuitiion;

/*cloase the safe update*/
SET SQL_SAFE_UPDATES = 0;

/*change FT into fruit*/
update food.new_nuitiion
set Category = 'Fruit'
where Category = 'FR';

Select Category from food.new_nuitiion where Category = 'FR';

/*change BRGR into Grain*/
update food.new_nuitiion
set Category = 'Grain'
where Category = 'BRGR';

Select Category from food.new_nuitiion where Category = 'Grain';

/*change DA into Dairy*/
update food.new_nuitiion
set Category = 'Dairy'
where Category = 'DA';

Select Category from food.new_nuitiion where Category = 'DA';

/*change VG into Vegetble*/
update food.new_nuitiion
set Category = 'Vegetble'
where Category = 'VG';

Select Category from food.new_nuitiion where Category = 'VG';

/*change ME, Seafood, SENU into meat*/
update food.new_nuitiion
set Category = 'Meat'
where Category = 'ME' or
      Category = 'SF' or
      Category = 'SENU';

Select Category from food.new_nuitiion where Category = 'Meat';

/*change ALBE, DESW, FT, SO into Additional*/
update food.new_nuitiion
set Category = 'Additional'
where Category = 'ALBE' or
      Category = 'DESW' or
      Category = 'FT' or
      Category = 'SO';

Select Category from food.new_nuitiion where Category = 'Additional';

/*check 6 ingredents in Category table*/
Select distinct Category from food.new_nuitiion;
 
 /*open the safe update*/
 SET SQL_SAFE_UPDATES = 1;
 
 /*commit all the update*/
 commit;
 
 Select * from food.new_nuitiion;