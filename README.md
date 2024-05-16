# Window-Manufacturing-Process-Analysis

In this business scenario, we're tackling the challenge of predicting breakage rates in a window manufacturing process. To do this, we're using a linear regression model that relates factors like Ambient temperature, Cut speed, Glass thickness, Edge deletion rate, Silicon viscosity, and Window size to breakage rates. We make some assumptions, such as a linear relationship and the representativeness of historical data for future conditions.

The occurrence of breakages not only leads to financial losses but also impacts the overall efficiency and customer satisfaction. The key stakeholder and end-user of our decision support system (DSS) is the window manufacturing technician responsible for overseeing the production process.

-Methodology-

-Developed a stepwise linear regression model that relates factors like Ambient temperature, Cut speed, Glass thickness, Edge deletion rate, Silicon viscosity, and Window size to breakage rates while selectively identifying relevant variables using the stepwise approach. 

-To assess the predictive model's success, we use metrics like R-squared and Mean Squared Error (MSE).

-Developed an optimization model based on latter regression model to distinguish between controllable (adjustable) and non-controllable (external) variables as well as apply constraints to ensure a realistic and feasible solution.

-Optimization model is used to benchmark optimal ranges for the miscellaneous manufacturing settings thereby providing prescriptive support to various technicians operating the machines.
