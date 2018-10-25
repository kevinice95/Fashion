library(tidyverse)

dat <- read_csv('fictionfabricsc.csv')


dat <- dat %>% 
  select(-Counts) %>% 
  gather(key=word, value=count, -Year)

dat %>% group_by(word) %>%
  mutate(total = sum(count)) %>%
  ungroup() %>%
  filter(total > 0) %>% 
  mutate(count = log10(count+1)) %>%
  ggplot(aes(x=Year, y=count))+
  facet_wrap(~word) +
  geom_line()

dat %>% group_by(word) %>%
  mutate(total = sum(count)) %>%
  ungroup() %>%
  filter(total > 0) %>% 
  mutate(count = log10(count+1)) %>%
  group_by(Year) %>%
  mutate(m = mean(count)) %>%
  ungroup() %>%
  mutate(d.count = count-m) %>%
  group_by(word) %>%
  mutate(zd.count = (d.count-mean(d.count))/sd(d.count)) %>%
  ungroup() %>%
  ggplot(aes(x=Year, y=zd.count))+
  facet_wrap(~word, ncol=10) +
  geom_line()

  
zdffts <- dat %>% group_by(word) %>%
  mutate(total = sum(count)) %>%
  ungroup() %>%
  filter(total > 0) %>% 
  mutate(count = log10(count+1)) %>%
  group_by(Year) %>%
  mutate(m = mean(count)) %>%
  ungroup() %>%
  mutate(d.count = count-m) %>%
  group_by(word) %>%
  mutate(zd.count = (d.count-mean(d.count))/sd(d.count)) %>%
  do(.$count %>% 
       fft() %>% 
       abs() %>% 
       .[2:(length(.)/2)] %>%
       bind_cols(a=., n=1:length(.)))
  

ffts <- dat %>% 
  mutate(count = log10(count+1)) %>% 
  group_by(word) %>% 
  mutate(count = (count-mean(count))/sd(count)) %>%
  do(.$count %>% 
              fft() %>% 
              abs() %>% 
              .[2:(length(.)/2)] %>%
              bind_cols(a=., n=1:length(.)))

ffts %>% filter(n<=50) %>% 
  mutate(a=log10(a)) %>%
  group_by(word) %>%
  mutate(a = 0.5+a + 0.25*lag(a, n=1) + 0.25*lead(a, n=1)) %>%
  mutate(a = 0.5+a + 0.25*lag(a, n=1) + 0.25*lead(a, n=1)) %>%
  mutate(a = 0.5+a + 0.25*lag(a, n=1) + 0.25*lead(a, n=1)) %>%
  mutate(a = 0.5+a + 0.25*lag(a, n=1) + 0.25*lead(a, n=1)) %>%
  ggplot(aes(x=n, y=log10(a), color=word))+
  geom_line()
