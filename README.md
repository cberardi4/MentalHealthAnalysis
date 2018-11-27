# Mental Health in the Tech Workplace

Dataset: ​https://www.kaggle.com/osmi/mental-health-in-tech-survey

## What will our data tell us:
- Are tech companies creating a workspace that promotes positive mental health?
- What are the strongest predictors of mental health issues or certain attitudes towards
mental health in the workplace?
- Does openness about mental health create a more healthy environment?
- Are other countries promoting positive mental health better than the US?
## What is in the dataset:
This dataset contains information from a mental health survey of work environments at
technology companies, the resources the company provides for mental health, and the level of openness with employers about mental health issues based on different countries around the world.
How we can apply this to businesses:
Injury prevention has remained one of the top concerns and liabilities for many
employers for years. Mental wellness has been a topic that has been ignored throughout generations. A workplace environment has the ability to affect an employees mental health in a both a negative and positive light. We are interested in finding out what the overall reaction is to mental health in the workplace and what predictors in the data set have influence on this topic. From a business perspective, our discoveries may help us understand how to create a better stigma around mental health in a work environment.
## How we will analyze our data:
For the first question, we will find the variables that have the strongest correlation to
having a mental health issue. We will create two different models. First, we will use the treatment variable, and make the assumption that if you have sought help, then you have a mental health condition. We will find the variables with the strongest correlation to the treatment variable. Secondly, we will use regularization methods to find the variables that have the highest impact on the model.
Another model that we plan to create is one based on the work_interfere variable. This variable is another indicator that someone has mental health issues. From here, we will create a subset of just people who have said yes, and follow the same procedure of variable selection and finding correlations.
Once we know which variables are the highest indicators of mental health, we will try a linear regression and/or a decision tree model to find the best result.
