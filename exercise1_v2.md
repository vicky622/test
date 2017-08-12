    library(readr)
    library(ggplot2)
    gblds <- read_csv("~/Desktop/Austin/summer/predictive modeling2/hw1/greenbuildings.csv")
    gblds<-na.omit(gblds)
    attach(gblds)

    mean_rent = mean(gblds$Rent)
    median_rent=median(gblds$Rent)
    ggplot(gblds, aes(x=Rent))+geom_histogram()+geom_vline(xintercept=mean_rent,color="blue")+geom_vline(xintercept=median_rent,color="red")+xlab("Mean Rent") + ylab("Frequency")

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](exercise1_v2_files/figure-markdown_strict/unnamed-chunk-2-1.png)

    mean_rent

    ## [1] 28.42092

    median_rent

    ## [1] 25.2

##### Fig 1.The graph is the distribution of overall mean rent in our dataset. Mean rent is the blue line and median rent is the red line. The graph is right skewed.

    green_only = subset(gblds, green_rating==1)
    mean_green = mean(green_only$Rent)
    median_green = median(green_only$Rent)

    nongreen_only = subset(gblds, green_rating==0)
    mean_nongreen = mean(nongreen_only$Rent)
    median_nongreen = median(nongreen_only$Rent)

    ggplot(green_only, aes(x=Rent))+geom_histogram()+geom_vline(xintercept=mean_green,color="blue")+geom_vline(xintercept=mean_nongreen,color="red")+xlab("Mean Rent") + ylab("Frequency")

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](exercise1_v2_files/figure-markdown_strict/unnamed-chunk-3-1.png)

    mean_green

    ## [1] 29.97415

    mean_nongreen

    ## [1] 28.27323

##### Fig 2.Without controling any other variables, mean rent of green buildings is $1.7 per square feet higher than mean rent of non-green buildings. In the histogram, the blud line indicates the mean rent of green buildings and red line indicates the mean rent of the non-green buildings. We can see the gap between the means of two groups.

    classA_only = subset(gblds, class_a==1)
    mean_classA = mean(classA_only$Rent)

    classB_only = subset(gblds, class_b==1)
    mean_classB = mean(classB_only$Rent)

    classC_only = subset(gblds, class_a!=1&class_b!=1)
    mean_classC = mean(classC_only$Rent)

    mean_classA

    ## [1] 32.3132

    mean_classB

    ## [1] 26.42507

    mean_classC

    ## [1] 23.93409

    ggplot(data=classA_only, aes(x=Rent)) +
        geom_histogram(data = classA_only, fill = "red", alpha = 0.2) + geom_histogram(bins=30)+
        geom_histogram(data = classB_only, fill = "blue", alpha = 0.2)+geom_histogram(data = classC_only, fill = "black", alpha = 0.2) +geom_vline(xintercept=mean_classA,color="red")+geom_vline(xintercept=mean_classB,color="blue")+geom_vline(xintercept=mean_classC,color="black")+xlab("Mean Rent") + ylab("Frequency")

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](exercise1_v2_files/figure-markdown_strict/unnamed-chunk-4-1.png)

##### Fig 3. The mean rent per square feet of class A buildings is around $6 higher than that of class B buildings. The mean rent per square feet between class B and class C are similar. In the graph, the red, blue and black lines represent the mean rent per square feet of class A, class B and class C buildings respectfully. Since the difference between rents of class B and class are neglectable, we devided the data into class A and non class-A subset. Qualification is definitely one of the most important compounding variables that we should control for our predictions.

    old_only = subset(gblds, age>34)
    mean_old = mean(old_only$Rent)

    new_only = subset(gblds, age<=34)
    mean_new = mean(new_only$Rent)

    # ggplot(classA_only, aes(x=Rent))+geom_histogram(bins=50)+geom_vline(xintercept=mean_classA,color="blue")+geom_vline(xintercept=mean_classB,color="red")
    mean_old

    ## [1] 27.88587

    mean_new

    ## [1] 28.95052

    ggplot(data=old_only, aes(x=Rent)) + 
        geom_histogram(data = old_only, fill = "red", alpha = 0.2) + 
        geom_histogram(data = new_only, fill = "blue", alpha = 0.2) +geom_vline(xintercept=mean_old,color="red")+geom_vline(xintercept=mean_new,color="blue")+xlab("Mean Rent") + ylab("Frequency")

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](exercise1_v2_files/figure-markdown_strict/unnamed-chunk-5-1.png)

##### Fig 4. We use the median age of all the buildings to divide buildings into old and new groups. However, the graph indicates that the mean rent per square feet between old and new buildings are close to each other. We didn't see a significant difference. Thus, we decided age is not a comfounding variables and we will not use age as a indicator of our subset.

    classA_green = subset(classA_only, green_rating==1)
    classA_nongreen = subset(classA_only, green_rating==0)
    target_greenA=subset(classA_green, leasing_rate>10)
    target_nongreenA=subset(classA_nongreen, leasing_rate>10)

##### Our group highly agrees with "guru"'s opinions on occupancy ratio. We also think that building with low occupancy would add noise to our prediction, so we only use buildings with higher than 10% occupancy to make the comparision.

##### Additionally, we believe that we have to compare the green building and non-green building's rent in the same cluster. Cluster variable is important as it controls many other fators, such as geographic location, precipitation and amenities. Furthur more, we don't know where the company will build the house, so it's more conservative to compare the mean rent between green and non-green buildings within the same clusters.

##### Based on the previous discussion, we decided to make two scenarios: 1 Buildings in class A and with occupancy greater than 10%. 2 Buildings not in class A and with occupancy greater than 10%.

    classnoA_only = subset(gblds, class_b!=1)
    classnoA_green = subset(classnoA_only, green_rating==1)
    classnoA_nongreen = subset(classnoA_only, green_rating==0)
    target_greennoA=subset(classnoA_green, leasing_rate>10)
    target_nongreennoA=subset(classnoA_nongreen, leasing_rate>10)

    mean_nongreenA=aggregate(target_nongreenA[,5],by=list(target_nongreenA$cluster),mean)
    # plot(mean_nongreen,col="blue")
    mean_greenA=aggregate(target_greenA[,5],by=list(target_greenA$cluster),mean)
    dfnewA<-merge(mean_nongreenA,mean_greenA,by=c("Group.1"))
    dfnewA$diff<-dfnewA$Rent.x-dfnewA$Rent.y #(green-nongreen)
    plot(dfnewA$Group.1,dfnewA$diff,col="red",ylab="Difference of mean rent",xlab="cluster")
    abline(h=mean(dfnewA$diff), col="blue")

![](exercise1_v2_files/figure-markdown_strict/difference%20for%20class%20A-1.png)

    mean(dfnewA$diff)

    ## [1] -0.1262386

##### Fig 5.The blue line represents the average difference between rents of green and non-green buildings in class A. The difference is close to zero. We would conclude that the mean rent of a green house in class A would not earn owners extra money comparing to non-green house in class A.

    mean_nongreennoA=aggregate(target_nongreennoA[,5],by=list(target_nongreennoA$cluster),mean)
    # plot(mean_nongreen,col="blue")
    mean_greennoA=aggregate(target_greennoA[,5],by=list(target_greennoA$cluster),mean)
    dfnewB<-merge(mean_nongreennoA,mean_greennoA,by=c("Group.1"))
    dfnewB$diff<-dfnewB$Rent.y-dfnewB$Rent.x #green-nongreen
    plot(dfnewB$Group.1,dfnewB$diff,col="red",ylab="Difference of mean rent",xlab="cluster")
    abline(h=mean(dfnewB$diff), col="blue")

![](exercise1_v2_files/figure-markdown_strict/difference%20for%20class%20B-1.png)

    mean(dfnewB$diff)

    ## [1] 2.523087

##### Fig 6. The blue line represents the average difference between rents of green and non-green buildings in class B and C.The difference is around $2.52 which means that average rent of green buildings with class B and C quality is around $2.52 higher than the non-green building. Since our building will be 250,000 square feet, this would translate into additional $630,000 of revenue for each year, if the company builts a building with class B and C quality. The cost of the building is $5,000,000, it will take around 7.9 years to recuperate these costs. Since the median age of the building is 34 years, conservatively speaking, we are able to earn $630,000 more for around 26 years.

##### Generally speaking, building a green-building is a good financial investment if you plan to build it with class B or C quality.
