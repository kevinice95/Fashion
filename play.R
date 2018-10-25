library(tidyverse)

dat <- read_csv('fictionfabrics%.csv')
dat <- read_csv('fictionclothes%.csv')


dat <- dat %>% 
  select(-Counts) %>% 
  gather(key=word, value=count, -Year)

dat %>% group_by(word) %>%
  filter(count > 0) %>%
  mutate(total = sum(count)) %>%
  ungroup() %>%
  filter(total > 0) %>% 
  mutate(count = log10(count)) %>%
  ggplot(aes(x=Year, y=count))+
  facet_wrap(~word) +
  geom_point(size=0.1)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=90, hjust=-1, vjust=0),
        strip.text = element_text(face = "bold"))

dat %>% group_by(word) %>%
  filter(count > 0) %>%
  mutate(total = sum(count)) %>% 
  ungroup() %>%
  filter(total > 0) %>% 
  mutate(count = log10(count)) %>%
  group_by(Year) %>%
  mutate(m = mean(count)) %>%
  ungroup() %>%
  mutate(d.count = count-m) %>%
  group_by(word) %>%
  mutate(zd.count = (d.count-mean(d.count))/sd(d.count)) %>%
  ungroup() %>%
  ggplot(aes(x=Year, y=zd.count))+
  facet_wrap(~word, ncol=10) +
  geom_point(size=0.1)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=90, hjust=-1, vjust=0),
        strip.text = element_text(face = "bold"))

acfs <- dat %>% group_by(word) %>%
  mutate(count = ifelse(count <= 0, NA, count)) %>%
  mutate(total = sum(count, na.rm = T)) %>% 
  ungroup() %>%
  filter(total > 0) %>% 
  mutate(count = log10(count)) %>%
  group_by(Year) %>%
  mutate(m = mean(count, na.rm=T)) %>%
  ungroup() %>%
  mutate(d.count = count-m) %>%
  group_by(word) %>%
  mutate(zd.count = (d.count-mean(d.count, na.rm=T))/sd(d.count, na.rm = T)) %>%
  # ungroup() %>% filter(word == 'denim') %>%
  do(.$zd.count %>% 
       ts() %>%
    acf(na.action = na.pass) %>%
      broom::tidy())


acfs %>% ungroup() %>%
  ggplot(aes(x=lag, y=acf))+
  facet_wrap(~word, ncol=10) +
  geom_line()+
  theme_minimal()+
  geom_hline(yintercept = 0, color='red')+
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0),
        strip.text = element_text(face = "bold"))

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

zdffts %>% filter(n<=50) %>% 
  mutate(a=log10(a)) %>%
  group_by(word) %>%
  ggplot(aes(x=n, y=(a)))+
  geom_line()+
  facet_wrap(~word, ncol=10)
