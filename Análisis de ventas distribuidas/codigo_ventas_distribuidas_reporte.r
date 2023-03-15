AlmacenInventario <- mutate(AlmacenInventario,
	location_shop = sample(1:6, 2000, replace = TRUE))
#Establecer IDs aleatorias en el campo ubicación de tienda

AlmacenInventario <- mutate(AlmacenInventario,
	location_shop = ifelse(location_shop == 1, 'Peoria', ifelse(
	location_shop == 2, 'Charleston', ifelse(
	location_shop == 3, 'El Paso', ifelse(
	location_shop == 4, 'Midland', ifelse(
	location_shop == 5, 'Baltimore', ifelse(
	location_shop == 6, 'Boise', NA)))))))
#Cambiar los IDs por los nombres de las ciudades correspondientes

peo_shop <- filter(AlmacenInventario, location_shop == 'Peoria')
#Para identificar el número de registros que cuenta cada localidad de tienda y colocar las direcciones después

AlmacenInventario <- mutate(AlmacenInventario,
	salesman_name = ifelse(
	location_shop == 'Peoria', sample(1:3, 298, replace = TRUE), ifelse(
	location_shop == 'Charleston', sample(4:6, 334, replace = TRUE), ifelse(
	location_shop == 'El Paso', sample(7:9, 366, replace = TRUE), ifelse(
	location_shop == 'Midland', sample(10:12, 351, replace = TRUE), ifelse(
	location_shop == 'Baltimore', sample(13:15, 341, replace = TRUE), ifelse(
	location_shop == 'Boise', sample(16:18, 310, replace = TRUE), NA
	)))))))
#Asignar IDs de vendedores agrupados a 3 vendedores por tienda

AlmacenInventario <- mutate(AlmacenInventario,
	salesman_name = ifelse(
	salesman_name == 1, 'Ingelbert Boliver', ifelse(
	salesman_name == 2, 'Goddard Twiddy', ifelse(
	salesman_name == 3, 'Oran Eustis', ifelse(
	salesman_name == 4, 'Boyce Krikorian', ifelse(
	salesman_name == 5, 'Gorden Woolis', ifelse(
	salesman_name == 6, 'Bank Chataignier', ifelse(
	salesman_name == 7, 'Theressa Orbine', ifelse(
	salesman_name == 8, 'Em Oxtaby', ifelse(
	salesman_name == 9, 'Chris Andreone', ifelse(
	salesman_name == 10, 'Emlynne Filipson', ifelse(
	salesman_name == 11, 'Nanine Sandercock', ifelse(
	salesman_name == 12, 'Gerek Musterd', ifelse(
	salesman_name == 13, 'Jeannette Willavoys', ifelse(
	salesman_name == 14, 'Antonetta Tindall', ifelse(
	salesman_name == 15, 'Cullin Shorte', ifelse(
	salesman_name == 16, 'Mireille Stott', ifelse(
	salesman_name == 17, 'Kellina Sayward', ifelse(
	salesman_name == 18, 'Marlowe Cheshir', NA
	)))))))))))))))))))
#Cambiar las IDs de los nombres de vendedores por sus nombres

AlmacenInventario <- mutate(AlmacenInventario,
	location_customer = ifelse(
	location_shop == 'Peoria', sample(1:6, 298, replace = TRUE), ifelse(
	location_shop == 'Charleston', sample(7:12, 334, replace = TRUE), ifelse(
	location_shop == 'El Paso', sample(13:18, 366, replace = TRUE), ifelse(
	location_shop == 'Midland', sample(19:24, 351, replace = TRUE), ifelse(
	location_shop == 'Baltimore', sample(25:30, 341, replace = TRUE), ifelse(
	location_shop == 'Boise', sample(31:36, 310, replace = TRUE), NA
	)))))))
#Asignar IDs a la localidad de los clientes conectados a la ubicación cercana de las tiendas

AlmacenInventario <- mutate(AlmacenInventario,
	location_customer = ifelse(
	location_customer == 1, '1136 Bunker Hill Hill', ifelse(
	location_customer == 2, '6 Walton Street', ifelse(
	location_customer == 3, '65041 Milwaukee Drive', ifelse(
	location_customer == 4, '0454 Grover Parkway', ifelse(
	location_customer == 5, '703 Harbort Terrace', ifelse(
	location_customer == 6, '8064 Menomonie Alley', ifelse(
	location_customer == 7, '7 Spaight Crossing', ifelse(
	location_customer == 8, '0204 Chive Road', ifelse(
	location_customer == 9, '6315 Hoard Court', ifelse(
	location_customer == 10, '69745 Butternut Plaza', ifelse(
	location_customer == 11, '52543 Mallard Place', ifelse(
	location_customer == 12, '3016 Oak Valley Alley', ifelse(
	location_customer == 13, '601 Gina Trail', ifelse(
	location_customer == 14, '12 Buhler Street', ifelse(
	location_customer == 15, '87 Farragut Parkway', ifelse(
	location_customer == 16, '15917 Fremont Crossing', ifelse(
	location_customer == 17, '9981 Stoughton Way', ifelse(
	location_customer == 18, '117 Farmco Drive', ifelse(
	location_customer == 19, '58688 Atwood Circle', ifelse(
	location_customer == 20, '38525 Dorton Alley', ifelse(
	location_customer == 21, '04 Holy Cross Road', ifelse(
	location_customer == 22, '651 Mariners Cove Place', ifelse(
	location_customer == 23, '91659 Rutledge Plaza', ifelse(
	location_customer == 24, '597 Miller Way', ifelse(
	location_customer == 25, '42302 Erie Point', ifelse(
	location_customer == 26, '69139 Dahle Drive', ifelse(
	location_customer == 27, '4883 Armistice Terrace', ifelse(
	location_customer == 28, '18048 Sommers Alley', ifelse(
	location_customer == 29, '9259 Clemons Plaza', ifelse(
	location_customer == 30, '347 Tennessee Junction', ifelse(
	location_customer == 31, '65 Helena Way', ifelse(
	location_customer == 32, '3 Randy Plaza', ifelse(
	location_customer == 33, '627 Surrey Way', ifelse(
	location_customer == 34, '7 Welch Pass', ifelse(
	location_customer == 35, '52436 Jenifer Court', ifelse(
	location_customer == 36, '24288 Larry Alley', NA
	)))))))))))))))))))))))))))))))))))))
#Con los IDs asignados cambiamos los valores numéricos por los nombres de las direcciones de los clientes

address <- AlmacenInventario %>% count(AlmacenInventario$location_customer)
#Con ésto se visualizan las ciudades y cuantos registros tienen para asginarlos en la operación anterior

AlmacenInventario <- mutate(AlmacenInventario,
	customer_name = ifelse(
	location_customer == '1136 Bunker Hill Hill', sample(1:5, 47, replace = TRUE), ifelse(
	location_customer == '6 Walton Street', rep(c(6), times = 51), ifelse(
	location_customer == '65041 Milwaukee Drive', rep(c(7), times = 53), ifelse(
	location_customer == '0454 Grover Parkway', sample(8:10, 39, replace = TRUE), ifelse(
	location_customer == '703 Harbort Terrace', sample(11:14, 66, replace = TRUE), ifelse(
	location_customer == '8064 Menomonie Alley', sample(15:19, 42, replace = TRUE), ifelse(
	location_customer == '7 Spaight Crossing', sample(20:22, 50, replace = TRUE), ifelse(
	location_customer == '0204 Chive Road', sample(23:26, 58, replace = TRUE), ifelse(
	location_customer == '6315 Hoard Court', rep(c(27), times = 44), ifelse(
	location_customer == '69745 Butternut Plaza', sample(28:31, 61, replace = TRUE), ifelse(
	location_customer == '52543 Mallard Place', sample(32:34, 47, replace = TRUE), ifelse(
	location_customer == '3016 Oak Valley Alley', sample(35:39, 74, replace = TRUE), ifelse(
	location_customer == '601 Gina Trail', rep(c(40), times = 71), ifelse(
	location_customer == '12 Buhler Street', rep(c(41), times = 90), ifelse(
	location_customer == '87 Farragut Parkway', sample(42:44, 56, replace = TRUE), ifelse(
	location_customer == '15917 Fremont Crossing', sample(45:47, 50, replace = TRUE), ifelse(
	location_customer == '9981 Stoughton Way', rep(c(48), times = 51), ifelse(
	location_customer == '117 Farmco Drive', sample(49:54, 48, replace = TRUE), ifelse(
	location_customer == '58688 Atwood Circle', sample(55:56, 58, replace = TRUE), ifelse(
	location_customer == '38525 Dorton Alley', sample(57:59, 63, replace = TRUE), ifelse(
	location_customer == '04 Holy Cross Road', sample(60:62, 68, replace = TRUE), ifelse(
	location_customer == '651 Mariners Cove Place', sample(63:67, 56, replace = TRUE), ifelse(
	location_customer == '91659 Rutledge Plaza', sample(68:69, 66, replace = TRUE), ifelse(
	location_customer == '597 Miller Way', sample(70:72, 40, replace = TRUE), ifelse(
	location_customer == '42302 Erie Point', sample(73:77, 53, replace = TRUE), ifelse(
	location_customer == '69139 Dahle Drive', sample(78:79, 56, replace = TRUE), ifelse(
	location_customer == '4883 Armistice Terrace', rep(c(80), times = 51), ifelse(
	location_customer == '18048 Sommers Alley', sample(81:84, 49, replace = TRUE), ifelse(
	location_customer == '9259 Clemons Plaza', sample(85:86, 70, replace = TRUE), ifelse(
	location_customer == '347 Tennessee Junction', sample(87:89, 62, replace = TRUE), ifelse(
	location_customer == '65 Helena Way', NA, ifelse(
	location_customer == '3 Randy Plaza', rep(c(90), times = 35), ifelse(
	location_customer == '627 Surrey Way', sample(91:95, 39, replace = TRUE), ifelse(
	location_customer == '7 Welch Pass', rep(c(96), times = 58), ifelse(
	location_customer == '52436 Jenifer Court', NA, ifelse(
	location_customer == '24288 Larry Alley', sample(97:100, 72, replace = TRUE), NA
	)))))))))))))))))))))))))))))))))))))
#Aquí se asginan los IDs de los clientes en dependencia de la ubicación de su vivienda

females <- list(1,2,4,5,6,10,12,13,15,16,18,20,21,23,25,27,34,35,36,38,40,42,43,44,46,47,48,49,
51,52,53,55,58,59,60,62,63,65,67,68,71,72,73,75,79,82,83,85,86,87,91,93,94,95,96,97,99,100)
#Crear una lista con los IDs de los clientes de género femenino

males <- list(3,7,8,9,11,14,17,19,22,24,26,28,29,30,31,32,33,37,39,41,45,
50,54,56,57,61,64,66,69,70,74,76,77,78,80,81,84,88,89,90,92,98)
#Crear una lista con los IDs de los clientes de género masculino

AlmacenInventario$customer_name %in% females
#Verificar la coincidencia de los valores de la lista de mujeres

AlmacenInventario <- mutate(AlmacenInventario,
	customer_gender = ifelse(
		customer_name %in% females == TRUE, 'Female', ifelse(
		customer_name %in% males == TRUE, 'Male', NA)))
#Establecer los géneros de los clientes en sus respectivos IDs utilizando las listas creadas como referencia

AlmacenInventario <- mutate(AlmacenInventario,
	age = ifelse(
	customer_name %in% c(7,74) , 17, ifelse(
	customer_name %in% c(22), 18, ifelse(
	customer_name %in% c(6,23,33,48), 19, ifelse(
	customer_name %in% c(35,36,56,63,92,97), 20, ifelse(
	customer_name %in% c(69,82), 21, ifelse(
	customer_name %in% c(2,16,19,24,81), 23, ifelse(
	customer_name %in% c(91), 24, ifelse(
	customer_name %in% c(8,26,94), 25, ifelse(
	customer_name %in% c(12,44), 26, ifelse(
	customer_name %in% c(88), 27, ifelse(
	customer_name %in% c(10,32), 28, ifelse(
	customer_name %in% c(18,28,98), 29, ifelse(
	customer_name %in% c(42,70), 30, ifelse(
	customer_name %in% c(1,79), 31, ifelse(
	customer_name %in% c(13), 32, ifelse(
	customer_name %in% c(25,60), 33, ifelse(
	customer_name %in% c(59,73), 34, ifelse(
	customer_name %in% c(45,54,64,71), 35, ifelse(
	customer_name %in% c(37), 36, ifelse(
	customer_name %in% c(47,72,78), 37, ifelse(
	customer_name %in% c(17,77), 38, ifelse(
	customer_name %in% c(4,5,11,65,68), 39, ifelse(
	customer_name %in% c(89), 40, ifelse(
	customer_name %in% c(39,75), 41, ifelse(
	customer_name %in% c(62,80), 42, ifelse(
	customer_name %in% c(9,31,57), 44, ifelse(
	customer_name %in% c(29), 45, ifelse(
	customer_name %in% c(61), 46, ifelse(
	customer_name %in% c(58), 47, ifelse(
	customer_name %in% c(46,53,66), 48, ifelse(
	customer_name %in% c(21), 49, ifelse(
	customer_name %in% c(3,27,41), 51, ifelse(
	customer_name %in% c(51,84,99), 52, ifelse(
	customer_name %in% c(52,87,90), 53, ifelse(
	customer_name %in% c(43,86,96,100), 54, ifelse(
	customer_name %in% c(76), 55, ifelse(
	customer_name %in% c(55,67,85), 56, ifelse(
	customer_name %in% c(20,50,83), 57, ifelse(
	customer_name %in% c(14,34,49), 58, ifelse(
	customer_name %in% c(95), 59, ifelse(
	customer_name %in% c(30), 60, ifelse(
	customer_name %in% c(93), 61, ifelse(
	customer_name %in% c(15,38,40), 64, NA
))))))))))))))))))))))))))))))))))))))))))))
#Establecer las edades de los clientes según sus IDs

AlmacenInventario <- mutate(AlmacenInventario,
customer_name = case_when(
customer_name == 1~'Ginny Langdale',
customer_name == 2~'Lilith Sallery',
customer_name == 3~'Hiram Baxster',
customer_name == 4~'Phillie Bolzen',
customer_name == 5~'Georgianna Spooner',
customer_name == 6~'Anet Dunham',
customer_name == 7~'Clarence Matussevich',
customer_name == 8~'Arnold Matteucci',
customer_name == 9~'Guilbert Burdas',
customer_name == 10~'Carie Prendergrast',
customer_name == 11~'Ring Gerb',
customer_name == 12~'Susan Sterry',
customer_name == 13~'Lois Skillings',
customer_name == 14~'Jerrome Trownson',
customer_name == 15~'Grete MacKilroe',
customer_name == 16~'Thomasina Huttley',
customer_name == 17~'Drew Dahlgren',
customer_name == 18~'Abigael Anglish',
customer_name == 19~'Boycey Ocurran',
customer_name == 20~'Florri Gonnin',
customer_name == 21~'Maren Dickie',
customer_name == 22~'Christiano Corssen',
customer_name == 23~'Codie Trayhorn',
customer_name == 24~'Dimitry Joselovitch',
customer_name == 25~'Stormie Mattedi',
customer_name == 26~'Pasquale Gauden',
customer_name == 27~'Leisha Patis',
customer_name == 28~'Guy Pittford',
customer_name == 29~'Nate Swannick',
customer_name == 30~'Burnard Lytell',
customer_name == 31~'Wyn Garmey',
customer_name == 32~'Eugene Darree',
customer_name == 33~'Briano Jollye',
customer_name == 34~'Audie Salliere',
customer_name == 35~'Carly Janeczek',
customer_name == 36~'Lesli Guerriero',
customer_name == 37~'Jared McCreery',
customer_name == 38~'Tonye Pipe',
customer_name == 39~'Vaughn Gages',
customer_name == 40~'Waneta Beri',
customer_name == 41~'Jamil Platfoot',
customer_name == 42~'Rhoda Kimbell',
customer_name == 43~'Jaymee Goghin',
customer_name == 44~'Zelma Frarey',
customer_name == 45~'Wendel Brunel',
customer_name == 46~'Maxi Fendlow',
customer_name == 47~'Cathrin Finkle',
customer_name == 48~'Papagena Arnaud',
customer_name == 49~'Anne-marie Whelan',
customer_name == 50~'Tonnie Derisly',
customer_name == 51~'Tani Clyma',
customer_name == 52~'Gratiana Bonifazio',
customer_name == 53~'Felisha Gatiss',
customer_name == 54~'Galen Evelyn',
customer_name == 55~'Lonnie Henriksson',
customer_name == 56~'Sydney Axelbee',
customer_name == 57~'Eddie Moretto',
customer_name == 58~'Ashlen Upstell',
customer_name == 59~'Ailis Brabham',
customer_name == 60~'Fiorenze Barks',
customer_name == 61~'Pernell Laydel',
customer_name == 62~'Jacquetta Ibbitt',
customer_name == 63~'Kristan Yablsley',
customer_name == 64~'Fairlie Perocci',
customer_name == 65~'Phillis Kores',
customer_name == 66~'Giovanni Gebby',
customer_name == 67~'Charis Edney',
customer_name == 68~'Lenore Wakes',
customer_name == 69~'Oby Liggett',
customer_name == 70~'Gifford Theakston',
customer_name == 71~'Evangeline De Robertis',
customer_name == 72~'Mireielle Hymus',
customer_name == 73~'Kristyn Anthes',
customer_name == 74~'Lind Dance',
customer_name == 75~'Lynda Dooley',
customer_name == 76~'Sherwin Lavelle',
customer_name == 77~'Simon Land',
customer_name == 78~'Randi Leighton',
customer_name == 79~'Murial Baudinelli',
customer_name == 80~'Bernarr Rann',
customer_name == 81~'Norby Prier',
customer_name == 82~'Rosaline Breagan',
customer_name == 83~'Mellisa Plumbridge',
customer_name == 84~'Jimmie McNulty',
customer_name == 85~'Kendra Cullinan',
customer_name == 86~'Zaria Staddart',
customer_name == 87~'Marcia Connikie',
customer_name == 88~'Buddy Graalman',
customer_name == 89~'Elston Kubal',
customer_name == 90~'Reginauld Doxsey',
customer_name == 91~'Jacklin Grob',
customer_name == 92~'Kristofor Chanders',
customer_name == 93~'Margit Riddock',
customer_name == 94~'Fidelity Deporte',
customer_name == 95~'Alyce Mariner',
customer_name == 96~'Dael Giraudy',
customer_name == 97~'Wileen Everson',
customer_name == 98~'Waite Olford',
customer_name == 99~'Agneta McFetridge',
customer_name == 100~'Bridgette Sleney'
))
#Asignamos los nombres a los clientes con sus IDs

AlmacenInventario <- mutate(AlmacenInventario,
	product_code = sample(1:80, 2000, replace = TRUE))
#Crear IDs aleatorias para 80 productos distintos en los 2000 registros

AlmacenInventario <- mutate(AlmacenInventario,
	unit_price = case_when(
product_code %in% c(1,11,21,31,41,51,61,71)~5,
product_code %in% c(2,12,22,32,42,52,62,72)~8,
product_code %in% c(3,13,23,33,43,53,63,73)~10,
product_code %in% c(4,14,24,34,44,54,64,74)~12,
product_code %in% c(5,15,25,35,45,55,65,75)~15,
product_code %in% c(6,16,26,36,46,56,66,76)~20,
product_code %in% c(7,17,27,37,47,57,67,77)~25,
product_code %in% c(8,18,28,38,48,58,68,78)~30,
product_code %in% c(9,19,29,39,49,59,69,79)~40,
product_code %in% c(10,20,30,40,50,60,70,80)~50
))
#Asignar valores de los precios de productos segmentados en sus últimos dígitos

AlmacenInventario <- mutate(AlmacenInventario,
	product_code = case_when(
product_code == 1 ~ 'Product XC01',
product_code == 2 ~ 'Product XC02',
product_code == 3 ~ 'Product XC03',
product_code == 4 ~ 'Product XC04',
product_code == 5 ~ 'Product XC05',
product_code == 6 ~ 'Product XC06',
product_code == 7 ~ 'Product XC07',
product_code == 8 ~ 'Product XC08',
product_code == 9 ~ 'Product XC09',
product_code == 10 ~ 'Product XC10',
product_code == 11 ~ 'Product XC11',
product_code == 12 ~ 'Product XC12',
product_code == 13 ~ 'Product XC13',
product_code == 14 ~ 'Product XC14',
product_code == 15 ~ 'Product XC15',
product_code == 16 ~ 'Product XC16',
product_code == 17 ~ 'Product XC17',
product_code == 18 ~ 'Product XC18',
product_code == 19 ~ 'Product XC19',
product_code == 20 ~ 'Product XC20',
product_code == 21 ~ 'Product XC21',
product_code == 22 ~ 'Product XC22',
product_code == 23 ~ 'Product XC23',
product_code == 24 ~ 'Product XC24',
product_code == 25 ~ 'Product XC25',
product_code == 26 ~ 'Product XC26',
product_code == 27 ~ 'Product XC27',
product_code == 28 ~ 'Product XC28',
product_code == 29 ~ 'Product XC29',
product_code == 30 ~ 'Product XC30',
product_code == 31 ~ 'Product XC31',
product_code == 32 ~ 'Product XC32',
product_code == 33 ~ 'Product XC33',
product_code == 34 ~ 'Product XC34',
product_code == 35 ~ 'Product XC35',
product_code == 36 ~ 'Product XC36',
product_code == 37 ~ 'Product XC37',
product_code == 38 ~ 'Product XC38',
product_code == 39 ~ 'Product XC39',
product_code == 40 ~ 'Product XC40',
product_code == 41 ~ 'Product XC41',
product_code == 42 ~ 'Product XC42',
product_code == 43 ~ 'Product XC43',
product_code == 44 ~ 'Product XC44',
product_code == 45 ~ 'Product XC45',
product_code == 46 ~ 'Product XC46',
product_code == 47 ~ 'Product XC47',
product_code == 48 ~ 'Product XC48',
product_code == 49 ~ 'Product XC49',
product_code == 50 ~ 'Product XC50',
product_code == 51 ~ 'Product XC51',
product_code == 52 ~ 'Product XC52',
product_code == 53 ~ 'Product XC53',
product_code == 54 ~ 'Product XC54',
product_code == 55 ~ 'Product XC55',
product_code == 56 ~ 'Product XC56',
product_code == 57 ~ 'Product XC57',
product_code == 58 ~ 'Product XC58',
product_code == 59 ~ 'Product XC59',
product_code == 60 ~ 'Product XC60',
product_code == 61 ~ 'Product XC61',
product_code == 62 ~ 'Product XC62',
product_code == 63 ~ 'Product XC63',
product_code == 64 ~ 'Product XC64',
product_code == 65 ~ 'Product XC65',
product_code == 66 ~ 'Product XC66',
product_code == 67 ~ 'Product XC67',
product_code == 68 ~ 'Product XC68',
product_code == 69 ~ 'Product XC69',
product_code == 70 ~ 'Product XC70',
product_code == 71 ~ 'Product XC71',
product_code == 72 ~ 'Product XC72',
product_code == 73 ~ 'Product XC73',
product_code == 74 ~ 'Product XC74',
product_code == 75 ~ 'Product XC75',
product_code == 76 ~ 'Product XC76',
product_code == 77 ~ 'Product XC77',
product_code == 78 ~ 'Product XC78',
product_code == 79 ~ 'Product XC79',
product_code == 80 ~ 'Product XC80'
))
#Cambiar los valores numéricos asignados por códigos de producto como nombre

AlmacenInventario <- mutate(AlmacenInventario,
	stock_sold = case_when(
		customer_name != 'NA' ~ sample(3:25, 2000, replace = TRUE)))
#Asignar cantidades aleatorias de productos vendidos

AlmacenInventario <- mutate(AlmacenInventario,
	sale = unit_price*stock_sold)
#Con las cantidades vendidas y precios establecidos, multiplicamos éstos valores para obtener el valor de las ventas realizadas

AlmacenInventario <- mutate(AlmacenInventario, product_code = NA)
#Para éste punto el campo de producto es una lista de vectores entonces la limpiamos para que sea una variable
AlmacenInventario <- cbind(AlmacenInventario, cm2_product)
#Agregamos una columna con los datos originales que tenían los productos en el campo anterior
AlmacenInventario <- mutate(AlmacenInventario, product_code = code)
#Y luego asignamos los valores identicamente
AlmacenInventario <- AlmacenInventario[,-15]
AlmacenInventario <- AlmacenInventario[,-14]
#Con éstos dos eliminamos las últimas columnas que no necesitamos más
AlmacenInventario <- mutate(AlmacenInventario, datetime = NA)
#Eliminamos un registro que estaba en las fechas; ésto elimina todos los registros de la columna
AlmacenInventario <- mutate(AlmacenInventario,
	product_code = ifelse(
		sale > 0, product_code, NA
	)
)
AlmacenInventario <- mutate(AlmacenInventario,
	unit_price = ifelse(
		sale > 0, unit_price, NA
	)
)
AlmacenInventario <- mutate(AlmacenInventario,
	category = ifelse(
		sale > 0, category, NA
	)
)
#En cada uno de éstos procesos se vacían los campos mencionados que no cuentan con clientes

fechas <- seq(as.POSIXct('2018-01-01'),
	as.POSIXct('2022-12-31'),
	by = '30 mins')
set.seed(3487635)
#Crear una secuencia de fechas con márgenes de 30 minutos junto a una semilla aleatoria

random_fechas <- sample(fechas,
	size = 2000,
	replace = TRUE)
random_fechas <- as.data.frame(random_fechas)
#Aquí creamos un dataframe aleatorio de fechas

AlmacenInventario <- cbind(AlmacenInventario, random_fechas)
AlmacenInventario <- mutate(AlmacenInventario,
	datetime = random_fechas
)
AlmacenInventario <- AlmacenInventario[,-14]
#Unimos la columna de las fechas aleatorias junto a la tabla principal, igualamos los valores y luego eliminamos la columna extra

write.csv(AlmacenInventario, 'X:\\address\\address\\address\\archivo.csv', row.names = FALSE)
write.csv(almacentable, 'X:\\address\\address\\address\\archivo.csv', row.names = FALSE)
#Con ésto podemos tener un archivo csv del dataset completo
#Y usar ese mismo archivo (en caso que se haya actualizado externamente) u otro para generar o actualizar otro dataset en el software




#
#Trabajar con los packages: brew, data.table, datasets, dplyr, ggplot, graphics, grid, htmltools, htmlwidgets, knitr, labeling
#	 lattice, plotly, rcolorbrewer, rmarkdown, treemap, xtable, yaml

```{r setup, echo=FALSE}
almacentable <- read.csv('X:\\address\\address\\address\\archivo.csv')
```
#Con ésto agarramos un archivos csv local para que nuestra estudio (Rstudio & Rmarkdown) lo lea y podamos hacer reportes con ese dataset


#Código de Rmarkdown
---
title: "Reporte de ventas"
author: "Agustin Rafael Mata Vargas"
date: "25 de noviembre de 2022 - 12 de diciembre de 2022"
output:
  pdf_document: default
  html_document: default
---

```{r setup, echo=FALSE}
almacentable <- read.csv('C:\\Users\\GIT2\\Documents\\AlmacenInventario.csv')
```

```{r packages, echo=FALSE, include=FALSE}
library(dplyr)
library(magrittr)
library(knitr)
library(data.table)
library(datasets)
library(tidyr)
library(readr)
library(tibble)
library(scales)
library(formattable)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(magrittr)
library(DT)
library(kableExtra)
library(ggrepel)
library(hrbrthemes)
library(forcats)
library(crayon)
library(viridis)
```


## Visualización de datos estadísticos y gráficos

###1. Datos generales de la fuente

```{r almacentable, echo=FALSE}
knk1 <- summary(almacentable[c('cantidad', 'precio', 'venta', 'edad')])

colnames(knk1) <- c('Cantidad', 'Precio', 'Venta', 'Edad')
  #Nombrar las columnas con nuevas variables
knk1 %>% kbl() %>% kable_styling()
  #Estilizar una tabla de datos para mejor visualización
```


###2. Ingresos totales 2022

```{r stats_initials, echo=FALSE}
venta_2022 <- almacentable %>%
  filter(between(fecha, '2022-01-01 00:00:00', '2022-12-31 23:59:59'))
venta_2022a <- format(dollar(sum(venta_2022$venta), 2), nsmall = 2)
venta_2022b <- format(round(sum(venta_2022$venta), 2), nsmall = 2)
venta_2022b <- as.integer(venta_2022b)
      #Venta del último año


venta_2022c <- almacentable %>%
  filter(between(fecha, '2022-10-01 00:00:00', '2022-12-31 23:59:59'))
venta_2022d <- format(dollar(sum(venta_2022c$venta), 2), nsmall = 2)
venta_2022e <- format(round(sum(venta_2022c$venta), 2), nsmall = 2)
venta_2022e <- as.integer(venta_2022e)
venta_var1 <- percent((venta_2022e/venta_2022b), 2)
# Ventas de octubre a diciembre de 2022
venta_2022f <- almacentable %>%
  filter(between(fecha, '2022-07-01 00:00:00', '2022-09-30 23:59:59'))
venta_2022g <- format(dollar(sum(venta_2022f$venta), 2), nsmall = 2)
venta_2022h <- format(round(sum(venta_2022f$venta), 2), nsmall = 2)
venta_2022h <- as.integer(venta_2022h)
venta_var2 <- percent((venta_2022h/venta_2022b), 2)
# Ventas de julio a septiembre de 2022
venta_2022i <- almacentable %>%
  filter(between(fecha, '2022-04-01 00:00:00', '2022-06-30 23:59:59'))
venta_2022j <- format(dollar(sum(venta_2022i$venta), 2), nsmall = 2)
venta_2022k <- format(round(sum(venta_2022i$venta), 2), nsmall = 2)
venta_2022k <- as.integer(venta_2022k)
venta_var3 <- percent((venta_2022k/venta_2022b), 2)
# Ventas de abril a junio de 2022
venta_2022l <- almacentable %>%
  filter(between(fecha, '2022-01-01 00:00:00', '2022-03-31 23:59:59'))
venta_2022m <- format(dollar(sum(venta_2022l$venta), 2), nsmall = 2)
venta_2022n <- format(round(sum(venta_2022l$venta), 2), nsmall = 2)
venta_2022n <- as.integer(venta_2022n)
venta_var4 <- percent((venta_2022n/venta_2022b), 2)
# Ventas de enero a marzo de 2022
      #Variaciones de los trimestres del año


venta_2021 <- almacentable %>%
  filter(between(fecha, '2021-01-01 00:00:00', '2021-12-31 23:59:59'))
venta_2021a <- format(dollar(sum(venta_2021$venta), 2), nsmall = 2)
venta_2021b <- format(round(sum(venta_2021$venta), 2), nsmall = 2)
venta_2021b <- as.integer(venta_2021b)

variacion_2021 <- (venta_2022b/venta_2021b)
var <- percent(variacion_2021, 2)
var_2021 <- (1 - var)
      #Variación del año pasado


tiendaba_22 <- almacentable %>%
  filter(between(fecha, '2022-01-01 00:00:00', '2022-12-31 23:59:59') & ciudadtienda == 'Baltimore')
tiendaba_22a <- format(dollar(sum(tiendaba_22$venta), 2), nsmall = 2)
tiendabo_22 <- almacentable %>%
  filter(between(fecha, '2022-01-01 00:00:00', '2022-12-31 23:59:59') & ciudadtienda == 'Boise')
tiendabo_22a <- format(dollar(sum(tiendabo_22$venta), 2), nsmall = 2)
tiendach_22 <- almacentable %>%
  filter(between(fecha, '2022-01-01 00:00:00', '2022-12-31 23:59:59') & ciudadtienda == 'Charleston')
tiendach_22a <- format(dollar(sum(tiendach_22$venta), 2), nsmall = 2)
tiendael_22 <- almacentable %>%
  filter(between(fecha, '2022-01-01 00:00:00', '2022-12-31 23:59:59') & ciudadtienda == 'El Paso')
tiendael_22a <- format(dollar(sum(tiendael_22$venta), 2), nsmall = 2)
tiendami_22 <- almacentable %>%
  filter(between(fecha, '2022-01-01 00:00:00', '2022-12-31 23:59:59') & ciudadtienda == 'Midland')
tiendami_22a <- format(dollar(sum(tiendami_22$venta), 2), nsmall = 2)
tiendape_22 <- almacentable %>%
  filter(between(fecha, '2022-01-01 00:00:00', '2022-12-31 23:59:59') & ciudadtienda == 'Peoria')
tiendape_22a <- format(dollar(sum(tiendape_22$venta), 2), nsmall = 2)
#Ventas de las tiendas del 2022

tiendaba_22_2 <- almacentable %>%
  filter(between(fecha, '2021-01-01 00:00:00', '2021-12-31 23:59:59') & ciudadtienda == 'Baltimore')
tiendaba_22b <- format(dollar(sum(tiendaba_22_2$venta), 2), nsmall = 2)
tiendabo_22_2 <- almacentable %>%
  filter(between(fecha, '2021-01-01 00:00:00', '2021-12-31 23:59:59') & ciudadtienda == 'Boise')
tiendabo_22b <- format(dollar(sum(tiendabo_22_2$venta), 2), nsmall = 2)
tiendach_22_2 <- almacentable %>%
  filter(between(fecha, '2021-01-01 00:00:00', '2021-12-31 23:59:59') & ciudadtienda == 'Charleston')
tiendach_22b <- format(dollar(sum(tiendach_22_2$venta), 2), nsmall = 2)
tiendael_22_2 <- almacentable %>%
  filter(between(fecha, '2021-01-01 00:00:00', '2021-12-31 23:59:59') & ciudadtienda == 'El Paso')
tiendael_22b <- format(dollar(sum(tiendael_22_2$venta), 2), nsmall = 2)
tiendami_22_2 <- almacentable %>%
  filter(between(fecha, '2021-01-01 00:00:00', '2021-12-31 23:59:59') & ciudadtienda == 'Midland')
tiendami_22b <- format(dollar(sum(tiendami_22_2$venta), 2), nsmall = 2)
tiendape_22_2 <- almacentable %>%
  filter(between(fecha, '2021-01-01 00:00:00', '2021-12-31 23:59:59') & ciudadtienda == 'Peoria')
tiendape_22b <- format(dollar(sum(tiendape_22_2$venta), 2), nsmall = 2)
#Ventas de las tiendas del 2021
      #Ventas y variaciones de tiendas con el año pasado

Baltimore <- c(tiendaba_22a, tiendaba_22b)
Boise <- c(tiendabo_22a, tiendabo_22b)
Charleston <- c(tiendach_22a, tiendach_22b)
El_Paso <- c(tiendael_22a, tiendael_22b)
Midland <- c(tiendami_22a, tiendami_22b)
Peoria <- c(tiendape_22a, tiendape_22b)
Variacion_Ventas <- percent(c(var, ''), 2)
Periodos <- c('2022', '2021')
Ventas <- c(venta_2022a, venta_2021a)
df <- data.frame(Periodos, Ventas, Variacion_Ventas, Baltimore, Boise, Charleston, El_Paso, Midland, Peoria)
  #Crear un dataframe con los vectores creados anteriormente

t(df) %>% kbl() %>% kable_styling()
```


###3. Número de compras y ventas aportadas de los clientes

```{r clientes_compras, echo=FALSE, message=FALSE}
clientes <- almacentable %>%
  group_by(clientes = almacentable$cliente) %>%
  summarise(
    ventas = as.numeric(format(round(sum(venta)))),
    no_ventas = as.numeric(format(round(NROW(venta))))
  ) %>%
  arrange(desc(no_ventas))
  #Una vez agrupados los datos al final se reorganizan según el número de ventas


cl1 <- clientes[order(clientes$no_ventas, decreasing = TRUE),]
cl1top <- head(cl1, 10)
cl2 <- clientes[order(clientes$no_ventas, decreasing = TRUE),]
cl2bottom <- tail(cl2, 10)
  #Recoger la información de los 10 mejores clientes y 10 peores

plottop <- ggplot(cl1top, aes(x = factor(no_ventas, levels = rev(levels(factor(no_ventas)))), y = clientes)) +
 geom_tile(aes(fill = ventas)) +
  geom_text(aes(label = format(dollar(ventas))), color = 'white') +
  ggtitle('Los 10 mayores compradores') +
  labs(x = 'Número de compras', y = '', fill = ' Ventas') +
  theme_dark()

bottomplot <- ggplot(cl2bottom, aes(x = factor(no_ventas, levels = rev(levels(factor(no_ventas)))), y = clientes)) +
 geom_tile(aes(fill = ventas)) +
  geom_text(aes(label = format(dollar(ventas))), color = 'white') +
  ggtitle('Los 10 menores compradores') +
    labs(x = 'Número de compras', y = '', fill = 'Ventas') +
      theme_dark()
  #En el anterior y actual gráfico de tabla con mapa de calor se posicionan los valores de venta en el centro de las celdas

plottop
bottomplot

```


###4. Valor porcentual de las ventas obtenidas por categoría cada año.

```{r categorías_ventas, message=FALSE, echo=FALSE}
cv <- almacentable %>%
  group_by(periodo = year(as.Date(fecha)), categorias = categoria) %>%
  summarise(ventas = format(dollar(sum(venta), 2), nsmall = 2)) %>%
  filter(periodo == '2022') %>%
  arrange(ventas)

per_cat <- almacentable %>% filter(year(as.Date(almacentable$fecha)) == '2022')
vc <- as.integer(sum(per_cat$venta))

per_aut1 <- almacentable %>%
  filter(categoria == 'Automotive' & year(as.Date(fecha)) == '2022')
v1 <- as.integer(sum(per_aut1$venta))
vt1 <- percent(v1/vc)

per_bea <- almacentable %>%
  filter(categoria == 'Beauty' & year(as.Date(fecha)) == '2022')
v2 <- as.integer(sum(per_bea$venta))
vt2 <- percent(v2/vc)

per_ele <- almacentable %>%
  filter(categoria == 'Electronics' & year(as.Date(fecha)) == '2022')
v3 <- as.integer(sum(per_ele$venta))
vt3 <- percent(v3/vc)  

per_fas <- almacentable %>%
  filter(categoria == 'Fashion' & year(as.Date(fecha)) == '2022')
v4 <- as.integer(sum(per_fas$venta))
vt4 <- percent(v4/vc)  

per_foo <- almacentable %>%
  filter(categoria == 'Food' & year(as.Date(fecha)) == '2022')
v5 <- as.integer(sum(per_foo$venta))
vt5 <- percent(v5/vc)  

per_fur <- almacentable %>%
  filter(categoria == 'Furniture' & year(as.Date(fecha)) == '2022')
v6 <- as.integer(sum(per_fur$venta))
vt6 <- percent(v6/vc)  

per_hea <- almacentable %>%
  filter(categoria == 'Health' & year(as.Date(fecha)) == '2022')
v7 <- as.integer(sum(per_hea$venta))
vt7 <- percent(v7/vc)  

per_toy <- almacentable %>%
  filter(categoria == 'Toys' & year(as.Date(fecha)) == '2022')
v8 <- as.integer(sum(per_toy$venta))
vt8 <- percent(v8/vc)  

percents <- c(vt6, vt2, vt1, vt8, vt4, vt7, vt3, vt5)
cv_complete <- cbind(cv, percents)
colnames(cv_complete)[4] <- 'valores'
  #Se agrega una columna adicional al dataframe utilizado
cv_complete <- cv_complete %>%
  mutate(cv_complete, categorias = case_when (
    categorias == 'Food' ~ 'Alimentos',
    categorias == 'Electronics' ~ 'Electrónicos',
    categorias == 'Health' ~ 'Salud',
    categorias == 'Fashion' ~ 'Moda',
    categorias == 'Toys' ~ 'Juguetes',
    categorias == 'Automotive' ~ 'Automovilístico',
    categorias == 'Beauty' ~ 'Belleza',
    categorias == 'Furniture' ~ 'Inmuebles'
  ))
#Los textos de las categorías se cambiaron desde el idioma inglés en el que estaba originalmente

ggplot(cv_complete, aes(x = '', y = factor(valores), fill = reorder(categorias, +valores))) +
  geom_col(width = 1, color = 'white') +
  coord_polar(theta = 'y', start = 0) +
  geom_label_repel(aes(x = 1.4, label = valores),
            position = position_stack(vjust = 0.9),
            max.overlaps = 20,
            show.legend = FALSE) +
  theme_void() +
  theme(legend.position = 'bottom') +
  ggtitle('Ingresos % por categoría 2022') +
  labs(fill = 'Categorías')
  #Se oscurece el fondo para ver mejor el círculo, círculo que se formó a través de un gráfico de barras

```


###5. Ventas en el último trimestre del 2022

```{r ventas_periodos, echo=FALSE, warning=FALSE, message=FALSE}
vp2022 <- almacentable %>%
  filter(year(as.Date(almacentable$fecha)) == '2022') %>%
  arrange(fecha)

vp2022i <- almacentable %>%
  filter(year(as.Date(almacentable$fecha)) == '2022')
vp2022i <- format(round(sum(vp2022i$venta)))
vp2022i <- as.integer(vp2022i)

vp2022d <- almacentable %>%
  filter(year(as.Date(almacentable$fecha)) == '2022')
vp2022d <- format(dollar(sum(vp2022d$venta)))

vp2022 <- vp2022 %>% mutate(vp2022,
                  as.Date(fecha))
colnames(vp2022)[14] <- 'nt_fecha'
vp22 <- vp2022 %>%
  group_by(fechas = vp2022$nt_fecha) %>%
  summarise(ventas = format(round(sum(venta))))

vplot10 <- ggplot(vp22, aes(x = fechas, y = as.integer(ventas))) +
  #geom_line() +
  geom_point(size = 3, color = 'red') +
  geom_smooth(span = 0.2, se = FALSE, method = 'loess') +
  ggtitle('Ventas de octubre 2022') +
  labs(x = '', y = 'Ventas ($)') +
  theme_light() +
  scale_x_date(date_labels = '%m-%d', limits = as.Date(c('2022-10-01', '2022-10-31')))

vplot11 <- ggplot(vp22, aes(x = fechas, y = as.integer(ventas))) +
  #geom_line() +
  geom_point(size = 3, color = 'red') +
  geom_smooth(span = 0.2, se = FALSE, method = 'loess') +
  ggtitle('Ventas de noviembre 2022') +
  labs(x = '', y = 'Ventas ($)') +
  theme_light() +
  scale_x_date(date_labels = '%m-%d', limits = as.Date(c('2022-11-01', '2022-11-30')))

vplot12 <- ggplot(vp22, aes(x = fechas, y = as.integer(ventas))) +
  #geom_line() +
  geom_point(size = 3, color = 'red') +
  geom_smooth(span = 0.2, se = FALSE, method = 'loess') +
  ggtitle('Ventas de diciembre 2022') +
  labs(x = '', y = 'Ventas ($)') +
  theme_light() +
  scale_x_date(date_labels = '%m-%d', limits = as.Date(c('2022-12-01', '2022-12-31')))

grid.arrange(vplot10, vplot11, vplot12)
  #Se utilizaron gráficos de líneas como referencia para asemejarlos con los puntos que indican los valores y la proximidad de la línea de crecimiento

```


###6. Ventas aportadas por las categorías en 2022

```{r venta_categoria, echo=FALSE, warning=FALSE}
vc <- almacentable %>%
  filter(between(fecha, '2022-01-01', '2022-12-31'))
vc <- vc %>%
  mutate(vc, nt_fecha = as.Date(fecha))

vic <- vc %>%
  mutate(vc, categoria = case_when (
    categoria == 'Food' ~ 'Alimentos',
    categoria == 'Electronics' ~ 'Electrónicos',
    categoria == 'Health' ~ 'Salud',
    categoria == 'Fashion' ~ 'Moda',
    categoria == 'Toys' ~ 'Juguetes',
    categoria == 'Automotive' ~ 'Automovilístico',
    categoria == 'Beauty' ~ 'Belleza',
    categoria == 'Furniture' ~ 'Inmuebles'
  )) %>%
  group_by(categoria) %>%
  summarise(ventasi = as.integer(format(round(sum(venta)))),
            ventasd = format(dollar(sum(venta)))) %>%
  arrange(ventasi) %>%
  ggplot(aes(x = reorder(categoria, +ventasi), y = ventasi, fill = reorder(categoria, -ventasi))) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = ventasd), position = position_dodge(1), vjust = 2) +
  labs(x = '', y = 'Ventas ($)', fill = 'Categorías') +
  guides(x = FALSE)

vnc <- vc %>%
  mutate(vc, categoria = case_when (
    categoria == 'Food' ~ 'Alimentos',
    categoria == 'Electronics' ~ 'Electrónicos',
    categoria == 'Health' ~ 'Salud',
    categoria == 'Fashion' ~ 'Moda',
    categoria == 'Toys' ~ 'Juguetes',
    categoria == 'Automotive' ~ 'Automovilístico',
    categoria == 'Beauty' ~ 'Belleza',
    categoria == 'Furniture' ~ 'Inmuebles'
  )) %>%
  group_by(categoria) %>%
  summarise(ventasi = as.integer(format(round(sum(venta)))),
            nventas = as.integer(format(round(NROW(venta))))) %>%
  arrange(nventas) %>%
  ggplot(aes(x = reorder(categoria, +nventas), y = nventas, fill = reorder(categoria, -nventas))) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = nventas), position = position_dodge(1), vjust = 2) +
  labs(x = '', y = 'N° de ventas', fill = 'Categorías') +
  guides(x = FALSE)

grid.arrange(vic, vnc)

```


###7. Ventas por categoría 2021-2022

```{r ventas_anuales, echo=FALSE, warning=FALSE, message=FALSE}
vcy <- almacentable %>%
  filter(year(as.Date(fecha)) >= '2021')
vcy <- vcy %>%
  mutate(vcy, nt_fecha = as.Date(fecha))

vcy1 <- vcy %>%
  mutate(vcy, categoria = case_when(
    categoria == 'Food' ~ 'Alimentos',
    categoria == 'Electronics' ~ 'Electrónicos',
    categoria == 'Health' ~ 'Salud',
    categoria == 'Fashion' ~ 'Moda',
    categoria == 'Toys' ~ 'Juguetes',
    categoria == 'Automotive' ~ 'Automovilístico',
    categoria == 'Beauty' ~ 'Belleza',
    categoria == 'Furniture' ~ 'Inmuebles'
  ))

vcy2 <- vcy1 %>%
  group_by(Periodos = year(as.Date(fecha)), Categorias = categoria) %>%
  summarise(
    ventasi = as.integer(format(round(sum(venta)))),
    Ventas = format(dollar(sum(venta)))
            ) %>%
  arrange(Categorias, Periodos)

bea21 <- vcy2[3,3]
bea22 <- vcy2[4,3]
bea_var <- percent(bea22/bea21)
fas21 <- vcy2[7,3]
fas22 <- vcy2[8,3]
fas_var <- percent(fas22/fas21)
fur21 <- vcy2[11,3]
fur22 <- vcy2[12,3]
fur_var <- percent(fur22/fur21)
aut21 <- vcy2[1,3]
aut22 <- vcy2[2,3]
aut_var <- percent(aut22/aut21)
ele21 <- vcy2[5,3]
ele22 <- vcy2[6,3]
ele_var <- percent(ele22/ele21)
foo21 <- vcy2[9,3]
foo22 <- vcy2[10,3]
foo_var <- percent(foo22/foo21)
toy21 <- vcy2[15,3]
toy22 <- vcy2[16,3]
toy_var <- percent(toy22/toy21)
hea21 <- vcy2[13,3]
hea22 <- vcy2[14,3]
hea_var <- percent(hea22/hea21)

Comparativas <- c(NA,aut_var,NA,bea_var,NA,ele_var,NA,fas_var,
                 NA,foo_var,NA,fur_var,NA,hea_var,NA,toy_var)
Comparativas <- percent(Comparativas)
Comparativas <- as.data.frame(Comparativas)
vcy2last <- cbind(vcy2, Comparativas)
vcy2last <- vcy2last[, c(1,2,4,5)]

vcy2last %>% kbl() %>% kable_styling()

```


###8. Ventas de las tiendas 2022

```{r ventas_tiendas, echo=FALSE}
vt1 <- almacentable %>%
  filter(year(as.Date(fecha)) == '2022')
vt2 <- vt1 %>%
  mutate(vt1, nt_fecha = as.Date(fecha))

vt2 %>%
  ggplot(aes(x = ciudadtienda, y = venta, fill = ciudadtienda)) +
  geom_boxplot() +
  geom_jitter(color = 'gold', size = 1, alpha = 0.5) +
  ggtitle('Ingreso de las ventas por tienda 2022') +
  labs(x = '', y = 'Ventas ($)', fill = 'Ubicación de la tienda') +
  theme_light()

```


###9. Datos de ventas de los 8 vendedores en 2022 (Top/Bottom)

```{r ventas_vendedores, echo=FALSE}
vv1 <- almacentable %>%
  filter(year(as.Date(fecha)) == '2022')

vv2 <- vv1 %>%
  group_by(Vendedores = vendedor) %>%
  summarise(
    ventasd = format(dollar(sum(venta))),
    ventasi = as.integer(format(round(sum(venta)))),
    nventas = as.numeric(format(round(NROW(venta))))
  ) %>%
  arrange(desc(ventasi))

vvtop <- head(vv2,5)
vvbottom <- tail(vv2,5)

topplot <- ggplot(vvtop, aes(x = factor(nventas, levels = rev(levels(factor(nventas)))), y = Vendedores)) +
  geom_tile(aes(fill = ventasi)) +
  geom_text(aes(label = ventasd), color = 'white') +
  ggtitle('Mejores vendedores') +
  labs(x = 'Número de ventas', y = '', fill = 'Ventas ($)') +
  coord_fixed() +
  theme_dark()

botplot <- ggplot(vvbottom, aes(x = factor(nventas, levels = rev(levels(factor(nventas)))), y = Vendedores)) +
  geom_tile(aes(fill = ventasi)) +
  geom_text(aes(label = ventasd), color = 'white') +
  ggtitle('Peores vendedores') +
  labs(x = 'Número de ventas', y = '', fill = 'Ventas ($)') +
  coord_fixed() +
  theme_dark()

topplot
botplot

```