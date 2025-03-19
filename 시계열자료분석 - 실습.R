#============================================================
# title: "시계열 분석"
# subtitle: "Time Series Analysis"
# author: "Begas"
#============================================================
library(forecast)
library(tseries)
library(ggplot2)
library(reshape)
library(zoo)

#============================================================
# Data Load
# - 1949년 ~ 1960년 까지의 월별 비행기 탑승 고객 수
#============================================================ 
origin <- AirPassengers
origin

#============================================================
# EDA
#============================================================ 

# 시도표
plot(origin)
# 데이터에 이분산이 존재함을 확인일 수 있음
# 분산 안정화를 위한 데이터 변환 필요

#분산 안정화를 위한 BoxCox 변환
lambda <- BoxCox.lambda(origin)
tran_org <- BoxCox(origin, BoxCox.lambda(origin))
plot(tran_org)
# BoxCox 변환 이후 이분산의 효과가 줄어 든것을 확인

# 정규성 및 Corr
# Hist Plot
hist(tran_org,prob=TRUE,12)
lines(density(tran_org))
# Q-Q PLOT
qqnorm(tran_org)
qqline(tran_org)

# 상관관계 확인
lag.plot(tran_org,12,do.lines=FALSE)
#전반적으로 데이터는 정규분라고 하기는 어려움.
#시차가 1,12일때 상관관계가 높음

#============================================================
# 시계열 분해 및 회귀분석 이용 예측
#============================================================ 

#분해법 : 가법모형 
stl_tran_org <- stl(tran_org, s.window = 12)

plot(stl_tran_org)
# 1차 Trend와 Seasonality 존재
# 잔차는 White Noise로 판단


# 회귀모형 
# 계절형 Dummy 변수 생성
M <- factor(cycle(tran_org))
M
stl_tran_org_df <- as.data.frame(stl_tran_org$time.series)
head(stl_tran_org_df)

# 회귀 모형 생성
# 모형식 : tran_org=trend∗β1+M1∗d1+...+M12∗d12+ϵ
# 가변수를 회귀모형에 넣을 때는 절편을 포함하는 것과 포함하지 않는 것에 대한 회귀계수 의미가 달라짐을 유의
model_stl <- lm(formula = tran_org~0+ stl_tran_org_df$trend+M, na.action = NULL)
summary(model_stl)

# 잔차 검정
# time Plot
plot(resid(model_stl))
abline(h=0, col='grey', lty=2, lwd=2)

par(mfrow=c(1,2))
# Hist Plot
hist(resid(model_stl),prob=TRUE,12, main = "Histogram of residuals")
lines(density(resid(model_stl)), col='red', lwd=2)
# Q-Q PLOT
qqnorm(resid(model_stl))
qqline(resid(model_stl))
par(mfrow=c(1,1))
# Q-Q Plot과 Histogram을 확인하면 양쪽 끝이 두텁


# 잔차들간의 상관관계가 존재하는가에 대한 가설 검정 : DW test
library(lmtest)
dwtest(model_stl) #가설검정 결과 잔차들간의 1차 상관관계가 존재 : White noise 라고 할 수 없음.


# 회귀모형 예측 결과 확인
plot(spline(time(origin), origin),type='l',xlab='Time',ylab='Pop')

# 원 데이터 및 fitted 데이터의 비교
# BoxCox 역변환 필요 함
lines(InvBoxCox(model_stl$fitted.values, lambda = BoxCox.lambda(origin)), col='red')

mean((origin - InvBoxCox(model_stl$fitted.values, lambda = BoxCox.lambda(origin)))^2, na.rm = TRUE)  #MSE

# 다른방법의 회귀모형(이건 수업 시간에 안해도 될 거 같음)

t <- 1:length(origin)
model_t <- lm(formula = tran_org~ t +M, na.action = NULL)
summary(model_t) 
# model_stl 에서는 trend에 해당하는 회귀계수가 1에 가까웠는데, 
# model_t에서는 t 에 해당하는 회귀계수가 2.563 으로 trend값과 거의 유사하다. 
# 이렇게 시계열 모형을 적합할 때 회귀모형을 적합해도 되지만, 
# 이 경우 회귀모형에서의 모형 가정이 깨지게 되며 (오차의 독립성)
# 최소제곱합에 의해 구해진 회귀계수의 추정량이 더이상 베스트가 아니게 된다. (분산이 커질 수 있음)
# 따라서 시계열 데이터에 대해 회귀모형으로 적합하는 것은 적절하지 않음.

#============================================================
# 지수평활을 이용한 예측
#============================================================

plot(stl(origin, s.window=12))
# Trend 및 Seasonality 존재
# Holt-Winter 지수평활 모형이 적합

#HoltWinters 모형 생성
model_es <- HoltWinters(origin, seasonal = "multiplicative") #이분산성이 존재하기 때문에 승법모형을 적용해야 함. 
#model_es <- HoltWinters(origin, beta=F, gamma=F, seasonal = "multiplicative") #단순지수 평활 모형을 사용하고 싶다면, beta=F, gamma=F 추가.
#model_es <- HoltWinters(origin, gamma=F, seasonal = "multiplicative") #이중지수 평활 모형을 사용하고 싶다면, gamma=F 추가.

# 원 데이터 및 fitted 데이터의 비교
# plot
plot(spline(time(origin), origin),type='l',xlab='Time',ylab='Pop')
lines(model_es$fitted[,1], col='red')
# mse 125.5
mean((origin-model_es$fitted[,1])^2)

# 예측
plot(forecast(model_es, h=36))  #짙은 회색 : 95%신뢰구간, 옅은 회색 : 80% 신뢰구간


# 이동평균법 사용 방법
plot(origin)

library(TTR) #이동평균 SMA 함수 사용하기 위한 패키지 불러오기
originSMA3 <- SMA(origin,n=3) #window=3
originSMA3
lines(originSMA3, col='darkorange', lty=2, lwd=2)

originSMA8 <- SMA(origin,n=8) #window=8
lines(originSMA8, col='steelblue', lty=4, lwd=2)

originSMA12 <- SMA(origin,n=12) #window=12
lines(originSMA12, col='darkgreen', lty=2, lwd=2)

legend("topleft", c("m=3", "m=8", "m=12"), col=c("darkorange", "steelblue","darkgreen"), lty=c(2,4,2), lwd=2)


#============================================================
# ARIMA를 이용한 예측
#============================================================

# 데이터 탐색 및 모형식별
# 시도표
plot(origin)
# 데이터의 이분산과 1차 추세, 계정성분이 존재


# 우선 분산 안정화를 위한 Box Cox 변환

# 분산 안정화
tran_org <- BoxCox(origin, BoxCox.lambda(origin))
plot(tran_org)
plot(log(tran_org))  
#일반적으로 로그변환이 더 쉽기 때문에 로그변환을 훨씬 많이 사용
#이 정도면 로그변환 해도 되지만, Boxcox 변환이 아무래도 더 정확하니 Boxcox 사용


#계절성분이 있으므로 계절차분
tran_sdiff_org <- diff(tran_org, lag=12)
plot(tran_sdiff_org)

# ACF, PACF를 통한 탐색
layout(1:2)
acf(tran_sdiff_org, lag.max = 100)#ACF 값이 아주 천천히 감소하고 있으므로 차분이 필요함
pacf(tran_sdiff_org, lag.max=100)

# 차분이 필요한지 검정 : 단위근 검정  H0 : 단위근이 있다.(즉 차분이 필요하다) 
library(fUnitRoots)  # library for function adfTest
adfTest(tran_sdiff_org, lags = 1, type = "c")  
#유의수준 5%에서는 기각할 수 있지만, 유의수준 1%에서는 기각할 수 없음. 
#ACF 값이 아주 천천히 감소하고 있기 때문에 차분을 하는 것이 좋다고 판단 

# 차분 실행 
tran_sdiff_diff_org <- diff(tran_sdiff_org, lag=1)
par(mfrow=c(1,1))
plot(tran_sdiff_diff_org)
abline(h=0, lty=2)

tran_sdiff_diff_org <- diff(diff(tran_org), lag=12)
tran_sdiff_diff_org 
layout(1:2)
acf(tran_sdiff_diff_org, lag.max = 48)
# acf는 lag=1,3,12에서 0이 아닌값 가짐  비계절 시차 4부터 절단 -> MA(3), 계절 -> 시차 2부터 절단 SMA(1)
pacf(tran_sdiff_diff_org, lag.max = 48)
# 시차 1와 9에서 0보다 큰 값을 가짐 -> AR(3), 계절 : 시차2부터 절단 ->  SAR(1) 
# 정확한 모형을 찾기 위해
# auto.arima를 통해 aic가 최소가 되는 order 값 구함
# 이론적으로는 ACF/PACF 그림을 보고 차수를 결정할 수 있다고는 하지만
# 실제 데이터 분석에서는 그림을 보고 결정하는 것은 어렵다.
# 참고 정도로 사용할 수 있음

auto.arima(tran_sdiff_diff_org, max.p = 3, max.q=3, max.Q=1, trace = T)  #trace = T 옵션을 사용하면 모형 선택 과정 확인 가능
auto.arima(tran_org, max.p = 3, max.q=3, max.Q=1, trace = T)
# 두 개 모형의 MA, SMA 계수의 추정값이 동일
# 모형 구축

model_arima <- arima(tran_sdiff_diff_org, order=c(0,0,1), seasonal = list(order = c(0,0,1), period = 12))
model_arima <- arima(tran_org, order=c(0,1,1), seasonal = list(order = c(0,1,1), period = 12))

# 모형 검진
# 잔차 검정
tsdiag(model_arima)

# 독립성 검정
Box.test(model_arima$residuals, lag=1, type="Ljung-Box") #H0 : rho1 = 0
Box.test(model_arima$residuals, lag=5, type="Ljung-Box") #H0 : rho1 = ... = rho5 = 0
Box.test(model_arima$residuals, lag=10, type="Ljung-Box") #H0 : rho1 = ... = rho10 = 0

# 잔차의 독립성, 등분산성, 정규성 만족

# 원 데이터 및 fitted 데이터의 비교
par(mfrow=c(1,1))
plot(spline(time(origin), origin),type='l',xlab='Time',ylab='Pop')
lines(InvBoxCox(fitted(model_arima), BoxCox.lambda(origin)), col='red') #역변환 필요
mean((origin - InvBoxCox(fitted(model_arima), BoxCox.lambda(origin)))^2) #MSE

# 12개월 예측
arima_fit <- predict(model_arima, n.ahead=12) #BoxCox 변환 데이터 사용
lambda <- BoxCox.lambda(origin)
ts.plot(origin, xlim=c(1950,1965), ylim = c(0, 1000))
lines(InvBoxCox(arima_fit$pred, lambda),col="red")
lines(InvBoxCox(arima_fit$pred+1.96*arima_fit$se, lambda),col="blue",lty=1)
lines(InvBoxCox(arima_fit$pred-1.96*arima_fit$se, lambda),col="blue",lty=1)

