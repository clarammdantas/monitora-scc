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
                
                tags$div(
                    tags$h2('Análise dos Pagamentos Realizados pela Prefeitura de Santa Cruz do Capibaribe'),
                    tags$p(
                        'Essa página tem como objetivo empoderar o cidadão santa cruzense fornecendo meios para a análise dos gastos da prefeitura. Para isso, fornecemos visualizações interativas',
                        tags$br(),
                        'e comentadas dos pagamentos realizados. As informações usadas aqui são provenientes do portal da transparência do município, que datam desde o ano de 2013, e foram coletadas',
                        tags$br(),
                        'até o dia 28/09/2021. Além dos pagamentos, os dados também incluem informações detalhadas do empenho relativo a cada pagamento, e a base de dados completa está acessível',
                        tags$br(),
                        'em ',
                        tags$a(href = 'https://github.com/clarammdantas/tcc-scraper', 'tcc-scraper'), '.'
                        ),
                ),
                
                tags$div(
                    tags$h3('Como interagir com os gráficos?'),
                    tags$p('Cada gráfico dispõe de seletores no lado esquerdo onde você pode selecionar o ano ou categoria de interesse e, ao selecionar um valor, o gráfico exibe os dados',
                           br(),
                           'correspondentes aos filtros aplicados. Cada seção possui um texto explicativo de como ler e interpretar cada tipo de gráfico, e no lado superior direito abaixo',
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
                        'A seguir temos um gráfico de barras que mostra o total gasto em um ano em cada categoria cadastrada. Para visualizar o valor exato gasto em uma categoria, basta passar',
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
                    br(),
                    div(
                        class="graph_and_selector_display",
                        tags$style(".graph_and_selector_display { display: flex; flex-direction: row; gap: 5px; }"),
                        numericInput(inputId = "year",
                                     label = "Selecione o ano",
                                     value = 2021, min = 2013, max = 2021),
                        tags$div(
                            class="graph",
                            plotlyOutput(outputId = "bar_year_category"),
                            tags$style(".graph { margin-left: 25px; }")
                        )
                    ),   
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
                marker = list(hoverFormat="%.2")
            ) %>% 
            layout(
                title = "Pagamentos Classificados por Categoria e Ano",
                yaxis = list(title = ''),
                xaxis = list(title = "Valor em R$")
            )
        
        
    })
}

shinyApp(ui = ui, server = server)
