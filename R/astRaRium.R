#' astRaRium: Generate infinite Astrarium minigames.
#' @export
#' @examples
#' # https://rfunctions.blogspot.com/2021/05/astrarium2-r-game-about-linking-stars.html
#' @description Generate infinite minigames based on the Astrarium minigame, from "Dragon Age: Inquisition". You have to click on the stars (asterisks) to find a path that that reveals the same image as shown while not repeating connections already made once. When it begins (by typing "astRaRium()" ), it will ask you for the desired number of stars and the number of links between pairs of stars. A good number is 12 and 9, respectively.
#' @return You play the game. You can succeed or fail.
astRaRium <- function() {

  # Função para contar duplicatas sem o pacote 'data.table'
  count.dups <- function(DF) {
    # Transforma o data frame em strings para contar duplicatas de linhas
    DF_str <- apply(DF, 1, paste, collapse = ",")

    # Conta as ocorrências de cada linha única
    count_table <- base::table(DF_str)

    # Retorna o resultado como um data frame, com o valor da contagem
    result <- base::data.frame(Row = base::names(count_table), Count = base::as.numeric(count_table))

    return(result)
  }

  # Solicita o número de estrelas e links
  stars <- as.numeric(base::readline("How Many Stars? Choose from 5 to 28. "))
  links <- as.numeric(base::readline("How Many Links Between Stars? Choose from 1 to 10. "))

  # Inicializa as coordenadas x e y e os pools de valores disponíveis
  x <- NULL
  y <- NULL
  poolx <- base::c(0:100)
  pooly <- base::c(0:100)

  # Gera coordenadas únicas para as estrelas
  for (i in 1:stars) {
    x <- base::c(x, base::sample(poolx, 1))
    poolx <- poolx[-base::which(poolx %in% base::c((x[i] - 2):(x[i] + 2)))]
    y <- base::c(y, base::sample(pooly, 1))
    pooly <- pooly[-base::which(pooly %in% base::c((y[i] - 2):(y[i] + 2)))]
  }

  # Função para gerar sequência aleatória de links
  seq_gen_rand_total <- function(item_count) {
    last_item <- -1
    last_last_item <- -1
    rand_seq <- NULL
    item_total <- base::sum(item_count)
    item_prob <- base::cumsum(item_count) / item_total

    # Gera sequência aleatória
    while (base::sum(item_count) > 0) {
      r_n <- stats::runif(1, 0, 1)
      new_item <- base::which(r_n < item_prob)[1]

      if (new_item != last_item) {
        if (item_count[new_item] > 0) {
          rand_seq <- base::c(rand_seq, new_item)
          last_last_item <- last_item
          last_item <- new_item
          item_count[new_item] <- item_count[new_item] - 1
        }
      } else if (base::length(base::which(item_count != 0)) == 1 && base::which(item_count != 0)[1] == last_item) {
        return(0)
      }
    }

    # Evitar repetição de links anteriores
    noback <- 0
    for (j in 3:(links + 1)) {
      noback <- base::c(noback, rand_seq[j] == rand_seq[j - 2])
    }

    # Criar matriz de links
    reps <- NULL
    for (p in 1:(links + 1)) {
      if (p < links + 1) {
        reps <- base::rbind(reps, base::c(rand_seq[p], rand_seq[p + 1]))
        reps <- base::rbind(reps, base::c(rand_seq[p + 1], rand_seq[p]))
      }
    }

    # Contar duplicatas
    duplicate_counts <- count.dups(reps)

    # Se não houver duplicatas e o máximo de contagens for 1, retorne a sequência
    if (base::sum(noback) == 0 && base::max(duplicate_counts$Count) == 1) {
      return(rand_seq[1:(links + 1)])
    } else {
      return(0)
    }
  }

  # Gera uma sequência de links válida
  randlink <- 0
  while (base::sum(randlink) == 0 || base::table(randlink)[1] <= 1 || randlink[1] != randlink[base::length(randlink)]) {
    randlink <- seq_gen_rand_total(1:stars)
  }

  seqs <- randlink[1:(links + 1)]

  # Gera a lista de links
  reps <- NULL
  for (p in 1:(links)) {
    reps <- base::rbind(reps, base::c(seqs[p], seqs[p + 1]))
  }

  # Ordena as respostas previstas
  predanswer <- reps[base::order(reps[, 2], reps[, 1]),]

  # Configura o gráfico
  grDevices::dev.new(width = 40, height = 20)
  graphics::par(mfrow = c(1, 2))

  # Plota as estrelas e os links
  plot(0:100, 0:100, pch = NA, xlab = NA, ylab = NA, axes = FALSE)
  for (l in 1:(links)) {
    graphics::segments(x[seqs[l]], y[seqs[l]], x[seqs[l + 1]], y[seqs[l + 1]])
  }

  plot(0:100, 0:100, pch = NA, xlab = NA, ylab = NA, axes = FALSE)
  graphics::points(x, y, cex = 1, pch = 8)
  graphics::text(x, y + 3, cex = 0.8)

  # Pergunta se o usuário quer resolver ou ver a solução
  pick.one <- base::readline("To give a solution: type 1 and press ENTER. To see a solution: another number and ENTER. Answer: ")
  ifelse(pick.one == "", pick.one <- 2, NA)

  if (pick.one == 1) {
    NA
  } else {
    return(paste("Star", as.numeric(seqs)))
  }

  # Recebe a resposta do usuário
  seqans <- numeric(links + 1)
  seqans[1] <- graphics::identify(x, y, n = 1, plot = FALSE)

  for (i in 2:(links + 1)) {
    seqans[i] <- graphics::identify(x, y, n = 1, plot = FALSE)
    graphics::segments(x[seqans[i - 1]], y[seqans[i - 1]], x[seqans[i]], y[seqans[i]])
  }

  answer <- seqans
  if (length(answer) != length(seqs)) {
    return("Incorrect number of connected stars (n of links + 1)")
  }

  # Cria a resposta observada
  reps2 <- NULL
  for (p in 1:(links)) {
    reps2 <- base::rbind(reps2, base::c(answer[p], answer[p + 1]))
  }

  reps3 <- reps2[, 2:1]
  obsanswer <- reps2[base::order(reps2[, 2], reps2[, 1]),]
  obsanswer2 <- reps3[base::order(reps3[, 2], reps3[, 1]),]

  # Verifica se a resposta está correta
  if (base::length(obsanswer2 == predanswer) == base::sum(obsanswer2 == predanswer) ||
      base::length(obsanswer == predanswer) == base::sum(obsanswer == predanswer)) {
    return(base::print("CORRECT! YOU ARE GREAT!"))
  } else {
    base::print("WRONG! Try Again! See a solution:")
  }

  return(seqs)
}
