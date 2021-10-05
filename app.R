library(shiny)
library(shinythemes)
library(plotly)
library(tidyverse)
library(here)

data = read_csv(here('./data/payments_details.csv'), quote ="\"")
data = data %>% 
    mutate(data = as.Date(data, format = "%d/%m/%Y"),
           data_empenho = as.Date(data_empenho, format = "%d/%m/%Y"),
           ano = format(data, "%Y"))

categorias = levels(factor(data$funcao))

glimpse(data)
ui <- fluidPage(
    theme = shinytheme("cerulean"),
    navbarPage(
        "Monitora SCC",
        tabPanel(
            "Visualizações",
            tags$div(
                class = "main_div",
                tags$style(".main_div { display: flex; flex-direction: column; width: 100%; }"),
                
                div(
                    h2('Análise dos Pagamentos Realizados pela Prefeitura de Santa Cruz do Capibaribe'),
                    p(
                        'Essa página tem como objetivo empoderar o cidadão santa cruzense fornecendo meios para a análise dos gastos da prefeitura. Para isso, fornecemos visualizações interativas',
                        br(),
                        'e comentadas dos pagamentos realizados. As informações usadas aqui são provenientes do portal da transparência do município, que datam desde o ano de 2013, e foram coletadas',
                        br(),
                        'até o dia 31/08/2021. Além dos pagamentos, os dados também incluem informações detalhadas do empenho relativo a cada pagamento, e a base de dados completa está acessível',
                        br(),
                        'em ',
                        a(href = 'https://github.com/clarammdantas/tcc-scraper', 'tcc-scraper'), '.'
                        ),
                ),
                
                div(
                    tags$h3('Como interagir com os gráficos?'),
                    tags$p('Cada gráfico dispõe de seletores no lado esquerdo onde você pode selecionar o ano ou categoria de interesse e, ao selecionar um valor, o gráfico exibe os dados',
                           br(),
                           'correspondentes aos filtros aplicados. Cada seção possui um texto explicando como ler e interpretar cada tipo de gráfico, e no lado superior direito, abaixo',
                           br(),
                           'do título, existem opções mais avançadas de interação, dentre elas o download do gráfico (primeiro ícone, uma câmera), e zoom (ícone de lupa). Se após aplicar um',
                           br(),
                           'zoom você desejar voltar ao estado original do gráfico, basta clicar duas vezes sobre a plotagem.',
                           ),
                    tags$p(
                        'Em alguns casos a escala de valores é muito grande, e pode parecer que o valor de alguma categoria é zero, porém, você perceberá que se aplicar um zoom no gráfico,',
                        tags$br(),
                        'será possível ver o valor real do objeto em questão.'
                    )
                ),
                
                div(
                    h3('Quanto foi gasto por categoria em cada ano?'),
                    p(
                        'A seguir temos um gráfico de barras que mostra o total gasto em um ano para cada categoria cadastrada. Para visualizar o valor exato gasto em uma categoria, basta passar',
                        br(),
                        'mouse sobre a barra de interesse. Outro ponto a ressaltar é que nesse gráfico alguns dos valores para o ano de 2021 parecem ser zero, como é o caso das categorias',
                        br(),
                        'Agricultura, Cultura, Transporte, e Desporto e Lazer, no entanto, como mencionado anteriormente, se você fizer um zoom nessas barras, será possível ver que seus valores',
                        br(),
                        'não são zero.'
                    ),
                    p(
                        'Podemos perceber que, com exceção do ano de 2016, onde foram gastos apenas 46.9 mil reais com educação, essa categoria está entre os 4 maiores gastos em todos os anos.',
                        br(),
                        'Contudo, é interessante notar que apesar do alto investimento, segundo o INEP, o Índice de Desenvolvimento de Educação Básica, IDEB, que mensura a qualidade do aprendizado',
                        br(),
                        'nas escolas públicas, tem resultados abaixo da meta em todos anos de 2013 à 2019, para as séries do 4º ao 9º ano. Você pode visualizar as notas em cada ano através da página',
                        br(),
                        a(href = "http://ideb.inep.gov.br/resultado/", 'resultados IDEB'),
                        '. '
                    ),
                    p(
                        'Dentre as categorias que menos recebem investimento ao longo dos anos temos, Cultura, Transporte, e Desporte e Lazer.'
                    ),
                    br(),
                    div(
                        class="graph_and_selector_display",
                        tags$style(".graph_and_selector_display { display: flex; flex-direction: row; gap: 5px; }"),
                        tags$div(
                            class="graph",
                            plotlyOutput(outputId = "bar_year_category"),
                            tags$style(".graph { margin-right: 35px; }")
                        ),
                        numericInput(inputId = "year",
                                     label = "Selecione o ano",
                                     value = 2021, min = 2013, max = 2021)
                    ),
                    
                    br(),
                    
                    p(
                        'Abaixo, temos uma visualização que nos permite comparar o valor gasto por área ao longo dos anos. O tamanho de cada ponto é proporcional ao valor total gastor naquele ano',
                        br(),
                        'para aquela área. Para ver o valor exato do gasto, passe o mouse sobre o ponto de interesse. Você também pode selecionar as categorias que você deseja visualizar no gráfico, basta',
                        br(),
                        'selecionar os valores que você deseja remover da visualização na coluna da legenda no lado direito do gráfico.',
                        br()
                    ),
                    p(
                        'Podemos confirmar com a visualização abaixo que de fato a área da educação é uma das que mais recebe verba municipal. Em 2017 o valor destinado à educação chegou aos',
                        br(),
                        'R$ 57.36 milhões. Porém existem anos como 2013, 2016, e 2019 onde o valor destinado foi menor que R$7 milhões, valor bem inferior comparado aos outros anos.',
                    ),
                    br(),
                    div(
                        class="graph_and_selector_display",
                        tags$style(".graph_and_selector_display { display: flex; flex-direction: row; gap: 5px; }"),
                        tags$div(
                            plotlyOutput(outputId = "year_by_year_category")
                        )
                    ),
                    br()
                ),
                
                div(
                    h3('Como se comporta a distribuição dos gastos por categoria ao longo dos anos?'),
                    
                    p(
                        'Para essa análise, vamos observar três tipos de visualizações diferentes, boxplots, gráfico de dispersão, e o gráfico de densidade. Vamos segmentar cada visualização por',
                        br(),
                        'ano e por categoria. Ao selecionar o ano e categoria correspondente, os três gráficos exibirão os valores para os filtros determinados.'
                    ),
                    
                    br(),
                    
                    h4('Boxplot'),
                    
                    p(
                        'De maneira concisa, um boxplot nos fornece uma visualização de algumas medidas de sumário importantes e robustas, sendo elas a mediana, o 25º percentil (q1), o 75º percentil (q3)',
                        br(),
                        'mínimos e máximos, e valores extremos. A mediana é uma medida de centro, e indica que metade dos valores são menores que o valor da mediana; o 25º percentil é o valor para',
                        br(),
                        'o qual 25% dos valores presentes nos dados são menores que o valor do percentil; o 75º percentil, análogamente, é o valor para o qual 75% dos valores na base de dados é menor.',
                    ),
                    
                    p(
                        'No boxplot abaixo, os pontos que estão em vermelho são possíveis outliers (valores extremos que não se parecem com a distribuição dos outros pontos). Se você passar o mouse',
                        br(),
                        'sobre o ponto poderá ter informações mais detalhadas sobre ele, como favorecido, data, e o valor exato. Se essas informações não forem o suficiente, você pode visitar o ',
                        br(),
                        a(href = "https://santacruzdocapibaribe.pe.tenosoftsistemas.com.br/portal/v81/p_index_entidades/?municipio=47&represent=1", 'portal da transparência de Sta. Cruz do Capibaribe'),
                        'navegar em "Prefeitura / FMS / FMAS" > "Despesa" > "Despesa Detalhada Diária", e pesquisar na aba de pagamentos pelo favorecido',
                        br(),
                        'em questão. Se você não quiser ver os pontos que extrapolam o limite máximo do boxplot, desmarque a opção "Exibir valores extremos" à direita do gráfico.'
                    ),
                    
                    p(
                        'Para o ano de 2021 e categoria Administração, pelo gráfico abaixo, temos que a mediana do valor dos pagamentos é de R$1.280,50, o Q1 (ou 25º percentil) é de R$195,91, e',
                        br(),
                        'o Q3 (75º percentil) é R$5.700,00, ou seja, 75% dos pagamentos realizados para essa categoria no ano de 2021 é menor ou igual a R$5.700,00. O maior valor extremo',
                        br(),
                        'foi de um pagamento que tem como favorecido a CELPE - Companhia Energética de Pernambuco, no valor de R$315.336,40. Um possível outlier é um pagamento realizado',
                        br(),
                        'à Tributus Informática LTDA, no valor de R$16.695,00.'
                    ),
                    
                    div(
                        class="graph_and_selector_display",
                        tags$style(".graph_and_selector_display { display: flex; flex-direction: row; gap: 30px; }"),
                        div(
                            id = "boxplot_graph",
                            tags$style('#boxplot_graph { margin-right: 30px; }'),
                            plotlyOutput(outputId = "boxplot")
                        ),
                        div(
                            checkboxInput(inputId = "extremes", "Exibir valores extremos", value = TRUE),
                            numericInput(inputId = "year_boxplot",
                                         label = "Selecione o ano",
                                         value = 2021, min = 2013, max = 2021),
                            selectInput(inputId = "category_boxplot", "Seleciona a área da despesa:", 
                                        choices = categorias)
                        )
                    )
                ),
                
                div(
                    h4('Gráfico de Densidade'),
                    
                    p(
                        'O gráfico de densidade nos informa a probabilidade de ocorrência de um certo valor e nos permite visualizar a distribuição de uma variável numérica que nesse caso são',
                        br(),
                        'os valores dos pagamentos realizados pela prefeitura. E como podemos observar, para a maioria dos anos a distribuição é enviesada à esquerda e possui uma cauda longa',
                        br(),
                        'à direita, ou seja, encontramos valores bem extremos, confirmando o que visualizamos no gráfico acima de boxplot.'
                    ),
                    
                    div(
                        class="graph_and_selector_display",
                        tags$style(".graph_and_selector_display { display: flex; flex-direction: row; gap: 5px; }"),
                        tags$div(
                            plotlyOutput(outputId = "distribution")
                        )
                    )
                ),
                
                div(
                    h4('Dispersão'),
                    
                    p(
                        'Por fim, o gráfico de dispersão nos fornece uma visão mais granular dos pagamentos ao longo do ano. Aqui você pode investigar melhor quais pagamentos foram realizados',
                        br(),
                        'ao longo de um mês por exemplo, ou ter uma visão maior dos possíveis outliers em contraste aos outros pontos. Podemos observar o valor mais extremo desse ano de 2021',
                        br(),
                        'que foi o pagamento realizado à CELPE e notar que ele diverge em mais de R$150.000,00 dos outros pagamentos realizados no ano. Podemos ver também que no mês de Agosto,',
                        br(),
                        'a prefeitura realizou pagamentos à empresas de tecnologia e artigos tecnológicos somando um valor total de R$22.871,50.'
                    ),
                    
                    div(
                        class="graph_and_selector_display",
                        tags$style(".graph_and_selector_display { display: flex; flex-direction: row; gap: 5px; }"),
                        tags$div(
                            plotlyOutput(outputId = "scatter_boxplot")
                        )
                    )
                ),
                
                div(
                    h3('Quais CNPJs mais receberam por ano?'),
                    
                    p(
                        'Aqui é mostrado os 10 maiores gastos com pessoas jurídicas por ano. Foi realizada a soma de todos os valores dos pagamentos realizados por CNPJ durante determinado ano.',
                        br(),
                        'É importante citar que para o CNPJ 11.196.515/0001-25 houve uma mudança no portal da transparência no nome do favorecido após a data de 31/08/2021 (data da última atualização',
                        br(),
                        'desses dados usados para estas análises). O nome do favorecido que constava até a data da nossa coleta de dados era FMSSCC PROG COMBATE COVID 19 - EFETIVOS, onde FMSSCC',
                        br(),
                        'significa Fundo Municipal de Saude de Santa Cruz do Capibaribe, e agora consta no portal da transparencia como FMSSCC SAD CONTRATADOS - PROCESSO SELETIVO, porém com mesmo',
                        br(),
                        'CNPJ.',
                    ),
                    
                    p(
                        'Dentre os fatos que chamam atenção, temos que o maior gasto de 2019 foi com o Gabinete do Vice Prefeito - Eletivo, correspondendo a um total de R$9.57 milhões. Esse valor é,',
                        br(),
                        'aproximadamente, 2.8 vezes maior que o valor gasto com a construtora Vialim que recebeu R$2.3 milhões da prefeitura no ano de 2019.'
                    ),
                    
                    div(
                        class="cnpj_maiores",
                        div(
                            id = "cnpj_max_graph",
                            tags$style('#cnpj_max_graph { margin-right: 30px; }'),
                            plotlyOutput(outputId = "cnpj_max")
                        ),
                        div(
                            numericInput(inputId = "year_max_cnpj",
                                         label = "Selecione o ano",
                                         value = 2021, min = 2013, max = 2021)
                        )
                    )
                )
            )
        )
    )
)

server <- function(input, output) {
    output$bar_year_category <- renderPlotly({
        payments_year_category <- data %>% 
            group_by(ano, funcao) %>% 
            filter(ano == input$year, !is.na(valor)) %>% 
            summarise(soma_pagamento = sum(valor), .groups = 'drop') %>% 
            plot_ly(
                x = ~soma_pagamento,
                y = ~reorder(funcao, soma_pagamento),
                type = "bar",
                text = ~paste('R$: ', round(soma_pagamento / 1e6, 2), 'M')
            ) %>% 
            layout(
                title = "Pagamentos Classificados por Categoria e Ano",
                yaxis = list(title = ''),
                xaxis = list(title = "Valor em milhões de R$")
            )
        
        
    })
    
    output$year_by_year_category <- renderPlotly({
        payments_by_year_and <- data %>% 
            group_by(ano, funcao) %>% 
            summarise(soma_pagamento = sum(valor), .groups = 'drop') %>% 
            plot_ly(
                x = ~ano,
                y = ~funcao,
                type = "scatter",
                mode = "markers",
                color = ~funcao,
                sizes = c(10, 50),
                size = ~soma_pagamento,
                marker = list(opacity = .5, sizemode='diameter'),
                text = ~paste('R$: ', round(soma_pagamento / 1e6, 2), 'M')
            ) %>% 
            layout(
                title = 'Total Gasto por Categoria ao Longo dos Anos',
                yaxis = list(title = ''),
                xaxis = list(title = 'Ano')
            )
    })
    
    output$boxplot <- renderPlotly({
        boxplot_by_category <- data %>% 
            group_by(funcao, ano) %>% 
            filter(funcao == input$category_boxplot, ano == input$year_boxplot) %>% 
            plot_ly(
                y = ~valor,
                boxpoints = "suspectedoutliers"
            ) %>% 
            add_boxplot(
                x = ~ano,
                marker = list(outliercolor = 'rgba(219, 64, 82, 0.6)'),
                jitter = 0.3,
                alpha = .4,
                text = ~paste(favorecido),
                boxpoints = input$extremes
            ) %>% 
            layout(
                title = 'Boxplot - Pagamentos por Área e Ano',
                xaxis = list(title = 'Ano'),
                yaxis = list(title = 'Valor em R$')
            )
    })
    
    output$scatter_boxplot <- renderPlotly({
        scatter_box <- data %>% 
            group_by(funcao, ano) %>% 
            filter(funcao == input$category_boxplot, ano == input$year_boxplot) %>% 
            plot_ly(
                x = ~data,
                y = ~valor,
                type = 'scatter',
                text = ~paste(favorecido),
                alpha = .4
            ) %>% 
            layout(
                title = 'Dispersão dos Pagamentos por Ano e Categoria',
                xaxis = list(title = ''),
                yaxis = list(title = 'Valor em R$')
            )
    })
    
    output$distribution <- renderPlotly({
        filtered_data = data %>% 
            filter(ano == input$year_boxplot, funcao == input$category_boxplot)
        
        density <- density(filtered_data$valor)
        
        distribution <- plot_ly(
            x = ~density$x,
            y = ~density$y,
            type = 'scatter',
            mode = 'lines',
            fill = 'tozeroy'
        ) %>% 
        layout(
            title = "Gráfico de Densidade - Pagamentos por Ano e Categoria",
            xaxis = list(title = 'Valor em R$'),
            yaxis = list(title = '')
        )
    })
    
    output$cnpj_max <- renderPlotly({
        scatter_cnpj <- data %>% 
            filter(tipo_1 == "Pessoa Jurídica") %>% 
            group_by(favorecido, ano, cpf_cnpj) %>% 
            summarise(total_pago = sum(valor), .groups = 'drop') %>% 
            filter(ano == input$year_max_cnpj) %>% 
            arrange(desc(total_pago)) %>% 
            slice(1:10) %>% 
            plot_ly(
                x = ~total_pago,
                y = ~reorder(favorecido, total_pago),
                type = 'scatter',
                color = 'coral',
                text = ~paste('R$: ', round(total_pago / 1e6, 2), 'M\n', 'CNPJ: ', cpf_cnpj)
            ) %>% 
            layout(
                title = 'As 10 Pessoas Jurídicas que Mais Receberam no Ano',
                xaxis = list(title = 'Valor total pago R$'),
                yaxis = list(title = '')
            )
    })
}

shinyApp(ui = ui, server = server)
