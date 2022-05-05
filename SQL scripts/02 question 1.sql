USE bibit_problem_test;

-- Verify the number of rows 
SELECT COUNT(*) FROM customers;
SELECT COUNT(*) FROM records;

-- preview the data
SELECT * FROM customers LIMIT 5;
SELECT * FROM records LIMIT 5;

-- find the relevant IDs
CREATE TEMPORARY TABLE customers_aged_17_to_30
SELECT
	user_id,
    user_age,
	CASE 
		WHEN user_age BETWEEN 17 AND 22 THEN 'group 1'
        WHEN user_age BETWEEN 23 AND 30 THEN 'group 2'
	END AS age_group
FROM customers
WHERE user_age BETWEEN 17 AND 30;

-- find records with users buy new investment
CREATE TEMPORARY TABLE records_buy
SELECT 
	records.*,
    customers_aged_17_to_30.age_group,
    customers_aged_17_to_30.user_age,
	(invested_amount - LAG(invested_amount, 1) OVER (PARTITION BY user_id, product ORDER BY record_date)) > 0 AS buy
FROM records
INNER JOIN customers_aged_17_to_30
	ON records.user_id = customers_aged_17_to_30.user_id;

-- find top 3 most active buying users aged 17-22
SELECT 
	user_id,
    user_age,
    SUM(buy) + 1 AS count -- add 1 for the first buy
FROM records_buy
WHERE age_group = 'group 1'
GROUP BY user_id
ORDER BY count DESC
LIMIT 10;

-- find top 3 most active buying users aged 23-30
SELECT 
	user_id,
    user_age,
    SUM(buy) + 1 AS count -- add 1 for the first buy
FROM records_buy
WHERE age_group = 'group 2'
GROUP BY user_id
ORDER BY count DESC
LIMIT 10;