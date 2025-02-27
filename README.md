# 제주 초등학교 스쿨존 교통안전 환경 분석

**기간**

**참여 인원**

**활동**

2023.03~2023.05 (3개월)

5명

교내 통계 분석 학회 팀 분석 프로젝트

## 📌 개요

---

- **문제 정의**
    - 제주도에서 스쿨존 교통사고 예방 정책을 시행하고 있으나 사고 감소 효과 미미함
    - 학교별 스쿨존 교통안전 환경을 고려하지 않고 일관적인 예방 정책을 시행하기 때문이라는 문제를 제기
    - 데이터를 기반으로 제주 스쿨존 교통안전 환경을 분석하여 예방 정책의 실효성을 검토하고 개선 방안을 제안
- **결과물**
    - 제주 초등학교 스쿨존 교통안전 환경 분석 및 인사이트
    - 제주 초등학교 스쿨존 교통사고 예방 정책 개선안
- **분석 도구**
    - R, Python
- **역할**
    - 데이터 수집 및 전처리
    - 분산 분석(ANOVA)
    - 발표 자료 제작 (PPT)

## 📌 분석 과정

---

### 1. 데이터 수집

---

- **활용 데이터**
    - 제주 초등학교 스쿨존 교통사고 데이터: 교통사고분석시스템(TAAS)에서 사고지점, 사고 건수 관련 정보를 웹크롤링을 통해 수집
    - 제주 어린이 보호구역 교통시설물, 도로 관련 데이터: 공공데이터포털에서 수집, 10개의 칼럼으로 구성
    - 제주 초등학교 스쿨존 교통환경 관련 설문조사 데이터: 공공데이터포털에서 수집, 2262명의 응답자와 70개의 문항으로 구성

### 2. 데이터 전처리

---

- **스쿨존 교통사고 지점 데이터 전처리**
    - 카카오 API를 활용해 사고 지점을 좌표계에서 주소로 변환
- **교통시설물, 도로 관련 데이터 전처리**
    - 초등학교의 위경도와 어린이보호구역 교통시설물, 도로의 위경도 간의 거리가 300m 이하인 경우 스쿨존 교통 환경과 관련이 있다고 판단
- **설문조사 데이터 전처리**
    - 자료형 변환: 설문조사 항목 중 순서형 자료를 대상으로 Ordinal Encoding 진행
    - 요인분석: 순서형 자료에 한정해 15개의 질문 항목을 전처리

![250226 1.png](250226_1.png)

![250226 2.png](250226_2.png)

: Scree plot을 통해 14개의 질문 항목을 5개 요인으로 요약 가능함을 판단

: 요인 적재량을 이용해 질문 항목들을 유사한 요인으로 요약 - 자전거 요인, 도로 요인, 안전사고 위험도, 버스 요인, 횡단 요인으로 요약 가능

: 요인 계수를 통해 답변을 요인별 위험도 점수로 변환

### 3. 군집 분석

---

- **K-means 클러스터링을 이용한 초등학교 군집화**
    - 스쿨존 교통안전 환경이 유사한 초등학교끼리 군집으로 묶어 교통안전 환경의 특징을 살펴보기 위함
    - 도로폭, 보차분리비율, 신호등 개수, 교통표지 개수, 차로수를 기준으로 군집화 진행

![스크린샷 2025-02-26 154936.png](%EC%8A%A4%ED%81%AC%EB%A6%B0%EC%83%B7_2025-02-26_154936.png)

![스크린샷 2025-02-26 154947.png](%EC%8A%A4%ED%81%AC%EB%A6%B0%EC%83%B7_2025-02-26_154947.png)

: WCSS - 군집이 3개일 때 군집 내 분산의 감소폭이 가장 큼

: 3개의 군집으로 군집화 진행

- **군집 분석 결과 해석**

![스크린샷 2024-12-13 130953.png](%EC%8A%A4%ED%81%AC%EB%A6%B0%EC%83%B7_2024-12-13_130953.png)

| 군집 | 특징 |
| --- | --- |
| 군집1 | - 구도심 지역에 주로 위치
- 도로폭이 좁음
- 보차비율이 낮음 |
| 군집2 | - 신도심 지역에 주로 위치
- 도로폭이 넓음
- 차로수가 많음
- 신호등 및 교통표지가 많음 |
| 군집3 | - 교외지역에 주로 위치
- 보차분리비율이 낮음
- 학생 수 대비 교통사고 건수가 높음 |

### 4. 분산 분석(ANOVA)

---

- **분산 분석(ANOVA)를 이용한 초등학교 군집 간 요인별 위험도 점수 비교**
    - 초등학교 군집 간 요인별 위험도 점수에 차이가 존재하는지를 확인하기 위함
    - 귀무가설: 군집 간 위험도 점수의 평균 값이 동일하다 vs 대립가설: 군집 간 위험도 점수의 평균 값이 동일하지 않다
    - 유의수준 0.05 하에 검정 진행
    - 사후 검정(Bonferroni t-test)를 통해 차이가 존재하는 군집 파악
    
    | 요인 | p-value | 결론 | 군집 간 차이 |
    | --- | --- | --- | --- |
    | 자전거 요인 | 0.000228 | 귀무가설 기각
    → 군집 간 위험도 점수 평균 값이 동일하지 않음  | 군집 3에서 유의한 차이가 존재
    → 군집 3 > 군집 2 = 군집 1 |
    | 도로 요인 | 0.0636 | 귀무가설 기각하지 못함
    → 군집 간 위험도 점수 평균 값이 동일함 |  |
    | 안전사고 위험도 | 0.979 | 귀무가설 기각하지 못함
    → 군집 간 위험도 점수 평균 값이 동일함 |  |
    | 버스 요인 | 0.0345 | 귀무가설 기각
    → 군집 간 위험도 점수 평균 값이 동일하지 않음  | 모든 군집에서 유의한 차이가 존재 
    → 군집 3 > 군집 1 > 군집 2 |
    | 횡단 요인 | 0.00814 | 귀무가설 기각
    → 군집 간 위험도 점수 평균 값이 동일하지 않음  | 군집 3에서 유의한 차이가 존재
    → 군집 1 = 군집 2 > 군집 3 |

## 📌 결론

---

- **제주 초등학교 스쿨존 교통안전 환경 분석 및 인사이트**
    - 초등학교마다 스쿨존 교통안전 환경에 차이가 존재함을 확인
    
    | 군집 | 교통안전 환경 특징 |
    | --- | --- |
    | 군집 1 | - 구도심 지역에 주로 위치
    - 도로폭이 좁고 보차비율이 낮음
    - 횡단 시 교통 사고 위험이 높음 |
    | 군집 2 | - 신도심 지역에 주로 위치
    - 교통량이 많음
    - 불법 주정차 차량이 많음
    - 횡단 시 교통 사고 위험이 높음 |
    | 군집 3 | - 교외지역에 주로 위치
    - 보차분리비율이 낮음
    - 자전거 이용 시 교통 사고 위험이 높음
    - 버스 이용 시 교통 사고 위험이 높음 |
- **제주 초등학교 스쿨존 교통사고 예방 정책 개선안**
    - 모든 초등학교에 동일한 교통사고 예방 정책을 시행하는 것보다 각 교통안전 환경을 반영한 예방 정책을 시행하는 것이 효과적일 것으로 기대
    
    | 군집 | 교통사고 예방 정책 개선안 |
    | --- | --- |
    | 군집 1 | - 어린이 통학로 조성, 노란색 횡단보도 도입 등 교통 안전 시설물 마련
    - 학생들의 등하교, 방과후 시간에 횡단 도우미 운영 |
    | 군집 2 | - 불법 주정차 단속 강화 및 인도 확충
    - 어린이 통학로 조성, 노란색 횡단보도 도입 등 교통 안전 시설물 마련
    - 학생들의 등하교, 방과후 시간에 횡단 도우미 운영 |
    | 군집 3 | - 보차분리 시급
    - 버스 승차 시 사고 방지를 위한 중앙분리대 설치
    - 자전거도로 확충
    - 통학버스 운영 |

## 📌 배운 점

---

- **웹크롤링을 활용한 데이터 수집 방법**
    - Beautiful Soup을 이용해 필요한 데이터를 웹에서 수집하는 프로세스를 배울 수 있었다.
- **요인 분석을 적용한 설문조사 데이터 분석 방법**
    - 설문조사 데이터의 개별 항목을 단순 비교하기보다 요인 분석을 통해 응답자의 주요 패턴을 요약하고 해석하는 방법을 배웠다.
- **데이터 해석 및 인사이트 도출 방법**
    - 데이터 분석 과정에서 신호등, 교통표지 개수 및 차로 수가 많은 군집에서 교통량이 많은 것으로 추정했다.
    - 추가적인 자료 조사를 통해 해당 군집이 실제로 교통량이 많고 불법 주정차 문제가 심하다는 사실을 발견했다. 이를 기반으로 추가적인 교통사고 예방 정책 개선안을 제안할 수 있었다.
    - 단순히 데이터 결과를 도출하는 것에서 끝나는 것이 아니라, 자료 조사를 통해 데이터 결과가 도출된 원인을 발견하여 더 풍부한 인사이트를 얻을 수 있음을 배웠다.