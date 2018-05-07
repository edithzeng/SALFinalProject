Create table businessLV (Select * from business where city="Las Vegas");
Create table review_coBu (Select * from review where business_id in (select id from businessLV_haveRe))
Create table Inspections_coBu (select * from Restaurant_Inspections where Address in (select address from businessLV_haveRe))
Create table Inspections_coBu (select * from Vegas_Inspection_cleansed where Address in (select address from businessesVegas))
Create table Business_coInspect (select * from businessesVegas where address in (select Address from Vegas_Inspection_cleansed))
Create table business_coAll (Select * from Business_coInspect where business_id in (select business_id from Vegas_Reviews));
DELETE FROM business_coAll WHERE address=''
Create table review_final (Select * from review where business_id in (select business_id from business_coAll));
select * from Inspections_final where `Current Grade` !='A' or `Inspection Grade` !='A'
select * from business_coAll where address not in (select Address from Inspections_final)
create table allequal(
		SELECT
			aggregate_missing_inspections.businessId
		FROM
			aggregate_missing_inspections,
			Inspections_final
		WHERE
			aggregate_missing_inspections.address = Inspections_final.Address
		AND aggregate_missing_inspections.`name` = Inspections_final.`Restaurant Name`
	)
create table coAll (select * from aggregate_missing_inspections,Inspections_final where aggregate_missing_inspections.address=Inspections_final.AddressInspection and aggregate_missing_inspections.`name`=Inspections_final.`Restaurant Name`)
CREATE TABLE InspectionGroup(
	SELECT
		*, count(*)
	FROM
		Inspection_notA
	GROUP BY
		Address,
		`Restaurant Name`
)UPDATE aggregate_missing_inspections
SET failTimes =(
	SELECT
		count
	FROM
		InspectionGroup
	WHERE
		InspectionGroup.Address = aggregate_missing_inspections.address
	AND substring(
		InspectionGroup.`Restaurant Name`,
		1,
		3
	)= SUBSTRING(
		aggregate_missing_inspections.`name`,
		1,
		3
	)
)
update aggregate_missing_inspections set ifFail=1 where failTimes !=0
CREATE TABLE Inspection_notA(
	SELECT
		*
	FROM
		Inspections_final
	WHERE
		`Current Grade` IN('B', 'C', 'X')
	OR `Inspection Grade` IN('B', 'C', 'X')
)