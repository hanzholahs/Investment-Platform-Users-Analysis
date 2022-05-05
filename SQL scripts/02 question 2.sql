USE bibit_problem_test;

-- Verify the number of rows 
SELECT COUNT(*) FROM customers;
SELECT COUNT(*) FROM records;

-- preview the data
SELECT * FROM customers LIMIT 5;
SELECT * FROM records LIMIT 5;

-- find relevant IDs
CREATE TEMPORARY TABLE customers_female_not_bisnis
SELECT 
	user_id,
    user_gender,
    user_income_source
FROM customers
WHERE user_gender = 'Female' AND 
	NOT user_income_source = 'Keuntungan Bisnis';
    
-- find records with users sell investment

CREATE TEMPORARY TABLE records_sell
SELECT 
	records.*,
    customers_female_not_bisnis.user_gender,
    customers_female_not_bisnis.user_income_source,
	(invested_amount - LAG(invested_amount, 1) OVER (PARTITION BY user_id, product ORDER BY record_date)) < 0 AS sell
FROM records
INNER JOIN customers_female_not_bisnis
	ON records.user_id = customers_female_not_bisnis.user_id
WHERE records.product = 'saham';

SELECT * FROM records_sell;

-- find top 3 most active female users with income source other than 'bisnis' selling 'saham'
SELECT 
	user_id,
    user_gender,
    user_income_source,
    SUM(sell) + 1 AS count -- add 1 for the first buy
FROM records_sell
GROUP BY user_id
ORDER BY count DESC
LIMIT 10;