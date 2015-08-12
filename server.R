library(shiny)
library(shinyAce)
library(exact2x2)
library(vcd)
library(reshape2)
library(coin)


shinyServer(function(input, output) {



#----------------------------------------------------
# 1. McNemar's Test (Raw data)
#----------------------------------------------------

    data1 <- reactive({
        
        dat <- read.csv(text=input$text1, sep="", na.strings=c("","NA","."))
        
            x <- table(dat)
            x <- addmargins(x)
            print(x)

        })
    
        output$data1.out <- renderPrint({
            data1()
        })
    
    
    
    
    
    test1 <- reactive({
        
        dat <- read.csv(text=input$text1, sep="", na.strings=c("","NA","."))
        
            x <- table(dat)
            res1 <- mcnemar.test(x)
            res2 <- mcnemar.exact(x)
            
            McNemarChi <- paste("McNemar's chi-squared = ", round(res1[[1]][[1]],3), ", ", "df = ", res1[[2]][[1]], sep = "")
            cat(sprintf(McNemarChi), "\n")
            print(res2)
            
       })
    
    output$test1.out <- renderPrint({
        test1()
    })
    
    
    
    
    
    makepPlot1 <- function(){
        
        dat <- read.csv(text=input$text1, sep="", na.strings=c("","NA","."))
        
            x <- table(dat)
            
            levI <- nrow(x) # 行の水準数
            levJ <- ncol(x) # 列の水準数
            dosu <- as.vector(t(x))
            
            # 標本比率の計算
            gokei <- c()
            bunbo <- c()
            for(i in 1:levI) # 各群の度数を集計
            {
                ds <- c()
                for(j in 1:levJ)
                {
                    ds <- c(ds, dosu[(i-1)*levJ+j])
                }
                gokei <- c(gokei, sum(ds))
                bunbo <- c(bunbo, rep(sum(ds), levJ))
            }
            hyohir <- dosu/bunbo # 群別の各値の比率
            
            zuhir <- c()
            for(i in levI:1) # 群ｉ→群１と逆順に並べ替える
            {
                for(j in 1:levJ)
                {
                    zuhir <- c(zuhir, hyohir[(i-1)*levJ+j] )
                }
            }
            
            zubar <- matrix(c(zuhir), nc=levJ, by=1)
            rownames(zubar) <- rev(rownames(x))
            colnames(zubar) <- colnames(x)
            #zubar <- zubar[nrow(zubar):1,]
            
            # プロット
            par(mar=c(5,6,2,4))
            barplot(t(zubar), hor=1, las=1, xlab="Percentage", col=gray.colors(ncol(x)))
            legend("bottomright", legend=colnames(zubar), fill=gray.colors(ncol(x)))
    }
    
    output$pPlot1 <- renderPlot({
        print(makepPlot1())
    })
    
    
    
    
    
    makemPlot1 <- function(){
       
       dat <- read.csv(text=input$text1, sep="", na.strings=c("","NA","."))
       
           x <- table(dat)
           mosaic(x, gp = shading_max, legend=FALSE, main="Mosaic plot")
       
    }
    
    output$mPlot1 <- renderPlot({
        print(makemPlot1())
    })
    
    
    
    
    
    info1 <- reactive({
        info1 <- paste("This analysis was conducted with ", strsplit(R.version$version.string, " \\(")[[1]][1], ".", sep = "")
        info2 <- paste("It was executed on ", date(), ".", sep = "")
        cat(sprintf(info1), "\n")
        cat(sprintf(info2), "\n")
    })
    
    output$info1.out <- renderPrint({
        info1()
    })










#----------------------------------------------------
# 2. McNemar's Test (Tabulated data)
#----------------------------------------------------

    data2 <- reactive({
        
        dat <- read.csv(text=input$text2, sep="", na.strings=c("","NA","."))
        
            x <- as.matrix(dat)
            x <- addmargins(x)
            print(x)

        })
    
        output$data2.out <- renderPrint({
            data2()
        })
    
    
    
    
    
    test2 <- reactive({
        
        dat <- read.csv(text=input$text2, sep="", na.strings=c("","NA","."))
        
            x <- dat
            x <- as.matrix(x)
            res1 <- mcnemar.test(x)
            res2 <- mcnemar.exact(x)
            
            McNemarChi <- paste("McNemar's chi-squared = ", round(res1[[1]][[1]],3), ", ", "df = ", res1[[2]][[1]], sep = "")
            cat(sprintf(McNemarChi), "\n")
            print(res2)
            
    })
    
    output$test2.out <- renderPrint({
        test2()
    })
    
    
    
    
    
    makepPlot2 <- function(){
        
        dat <- read.csv(text=input$text2, sep="", na.strings=c("","NA","."))
        
            x <- as.matrix(dat)
        
            levI <- nrow(x) # 行の水準数
            levJ <- ncol(x) # 列の水準数
            dosu <- as.vector(t(x))
            
            # 標本比率の計算
            gokei <- c()
            bunbo <- c()
            for(i in 1:levI) # 各群の度数を集計
            {
                ds <- c()
                for(j in 1:levJ)
                {
                    ds <- c(ds, dosu[(i-1)*levJ+j])
                }
                gokei <- c(gokei, sum(ds))
                bunbo <- c(bunbo, rep(sum(ds), levJ))
            }
            hyohir <- dosu/bunbo # 群別の各値の比率
            
            zuhir <- c()
            for(i in levI:1) # 群ｉ→群１と逆順に並べ替える
            {
                for(j in 1:levJ)
                {
                    zuhir <- c(zuhir, hyohir[(i-1)*levJ+j] )
                }
            }
            
            zubar <- matrix(c(zuhir), nc=levJ, by=1)
            rownames(zubar) <- rev(rownames(x))
            colnames(zubar) <- colnames(x)
            #zubar <- zubar[nrow(zubar):1,]
            
            # プロット
            par(mar=c(5,6,2,4))
            barplot(t(zubar), hor=1, las=1, xlab="Percentage", col=gray.colors(ncol(x)))
            legend("bottomright", legend=colnames(zubar), fill=gray.colors(ncol(x)))
    }
    
    output$pPlot2 <- renderPlot({
        print(makepPlot2())
    })
    
    
    
    
    
    makemPlot2 <- function(){
        
        dat <- read.csv(text=input$text2, sep="", na.strings=c("","NA","."))
        
        x <- as.matrix(dat)
        mosaic(x, gp = shading_max, legend=FALSE, main="Mosaic plot")
        
    }
    
    output$mPlot2 <- renderPlot({
        print(makemPlot2())
    })





    info2 <- reactive({
        info1 <- paste("This analysis was conducted with ", strsplit(R.version$version.string, " \\(")[[1]][1], ".", sep = "")
        info2 <- paste("It was executed on ", date(), ".", sep = "")
        cat(sprintf(info1), "\n")
        cat(sprintf(info2), "\n")
    })
    
    output$info2.out <- renderPrint({
        info2()
    })










#----------------------------------------------------
# 3. Cochran’s Q Test (Raw data)
#----------------------------------------------------

    data3 <- reactive({
        
        dat <- read.csv(text=input$text3, sep="", na.strings=c("","NA","."))
        
            dat[,1] <- factor(dat[,1])
            data.long <- melt(dat, idvars=dat[,1])
            x <- t(table(data.long$variable, data.long$value))
            x <- addmargins(x)
        
            print(x)
        })
    
        output$data3.out <- renderPrint({
            data3()
        })
    
    
    
    
    
    test3 <- reactive({
        
        dat <- read.csv(text=input$text3, sep="", na.strings=c("","NA","."))
                    
            dat[,1] <- factor(dat[,1])
            data.long <- melt(dat, idvars=dat[,1])
            q <- symmetry_test(data.long[,3] ~ factor(data.long[,2]) | factor(data.long[,1]), data=data.long, teststat="quad")
            
            CochranQChi <- paste("Cochran's Q chi-squared = ", round(q@statistic@teststatistic,3), ", ", "df = ", q@statistic@df, sep = "")
            cat(sprintf(CochranQChi), "\n")
            
            P.CochranQChi <- paste("p-value = ", pvalue(q), sep = "")
            cat(sprintf(P.CochranQChi), "\n", "\n")
            
            
            cat("Effect size for Cochran's Q test:", "\n")
            eta.squared.q <- q@statistic@teststatistic / (nrow(dat) * ((ncol(dat)-1)-1))
            ESQ <- paste("Eta-squared Q = ", round(eta.squared.q,3), sep = "")
            cat(sprintf(ESQ), "\n", "\n", "\n")
            
            cat("----------------------------------------", "\n", "Post-hoc test with McNemar's test:", "\n")
            pairMcNemar <- function(x) {
                p.val <- c()
                for (i in 1:length(x)) {
                    for (j in 1:length(x)) {
                        if (i >= j) {
                            next
                        } else {
                            
                            PairMcNemar <- table(x[,i], x[,j])
                            res1 <- mcnemar.test(PairMcNemar)
                            res2 <- mcnemar.exact(PairMcNemar)
                            p.val <- c(p.val, res2[[3]])
                            
                            cat("----------------------------------------", "\n")
                            cat("Comparison of", colnames(x)[i], "and", colnames(x)[j], ":", "\n",
                            "\n")
                            McNemarChi <- paste("McNemar's chi-squared = ", round(res1[[1]][[1]],3), ", ", "df = ", res1[[2]][[1]], sep = "")
                            cat(sprintf(McNemarChi), "\n")
                            
                            print(res2)
                            
                        }
                    }
                }
                padj <- c()
                padj <- p.adjust(p.val, "fdr")
                
                cat("----------------------------------------", "\n")
                cat("Adjusted p-value using false discovery rate [FDR]:", "\n", "\n")
                
                kochi<-c(); aite<-c()
                for(i in 1:length(x)){
                    for(j in 1:length(x)){
                        if (i >= j) {
                            next
                        } else {
                            kochi <- c(kochi, paste(colnames(x)[i], sep=""))
                            aite  <- c(aite,  paste(colnames(x)[j], sep=""))
                        }
                    }
                }
                
                a <- data.frame(Comparisons=paste(kochi, aite, sep=" & "), Adjusted.p.value=round(padj, 3))            
                print(a)
            }
            
            pairMcNemar(dat[,-1])
    
    })
    
    output$test3.out <- renderPrint({
        test3()
    })
    
    
    
    
    
    makepPlot3 <- function(){
        
        dat <- read.csv(text=input$text3, sep="", na.strings=c("","NA","."))
        
        dat[,1] <- factor(dat[,1])
        data.long <- melt(dat, idvars=dat[,1])
        x <- t(table(data.long$variable, data.long$value))
        n <- nrow(dat)
        prp <- round(((x/n)*100), 1)
        prp.rev <- apply(prp, 1, rev)
        
        par(mar=c(5,6,2,4))
        barplot(t(prp.rev), hor=1, las=1, xlab="Percentage")
        legend("bottomright", legend=rownames(x), fill=gray.colors(nrow(x)))
    }
    
    output$pPlot3 <- renderPlot({
        print(makepPlot3())
    })
    
    
    
    

    info3 <- reactive({
        info1 <- paste("This analysis was conducted with ", strsplit(R.version$version.string, " \\(")[[1]][1], ".", sep = "")
        info2 <- paste("It was executed on ", date(), ".", sep = "")
        cat(sprintf(info1), "\n")
        cat(sprintf(info2), "\n")
    })
    
    output$info3.out <- renderPrint({
        info3()
    })






})
