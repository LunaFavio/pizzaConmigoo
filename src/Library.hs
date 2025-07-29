module Library where
import PdePreludat

--PIZZA CONMIGO

--1)
--a Generar un data modelando la pizza.

data Pizza = Pizza {
    ingredientes :: [String],
    tamanio :: Number,
    calorias :: Number
}   deriving (Show, Eq)

--b Crear la función constante grandeDeMuzza, que es una pizza que tiene “salsa”,
--  “mozzarella” y “orégano”, tiene 8 porciones, y tiene 350 calorías.
grandeDeMuzza = Pizza ["salsa", "mozzarella", "oregano"] 8 350

--2) 
--   Calcular nivel de satisfacción que da una Pizza: 
--   0 si tiene palmito
--   cantidad de ingredientes * 80, siempre y cuando tenga menos de 500 calorías, en caso contrario es la mitad del cálculo.
--   Nota: Evitar repetición de lógica

nivelSatisfaccion (Pizza ingredientes tamanio calorias) 
    | elem "palmitos" ingredientes = 0
    | calorias >= 500 = calculoSatisfaccion
    | otherwise = calculoSatisfaccion / 2
    where calculoSatisfaccion = length ingredientes * 80

--3) 
-- Calcular el valor de una pizza, que es 120 veces la cantidad de ingredientes, multiplicado por su tamaño.

valorPizza (Pizza ingredientes tamanio _) = (tamanio*) . (120*) . length $ ingredientes
    --length ingredientes * 120 * tamanio

--4) 
--a nuevoIngrediente: Agrega un ingrediente a una pizza y 
--  agrega en calorías el doble de la cantidad de letras que tiene dicho ingrediente

nuevoIngrediente ingrediente pizza = 
    sumarCalorias nuevasCalorias pizza {
        ingredientes = ingrediente : ingredientes pizza
    }
    where nuevasCalorias = (2*) . length $ ingrediente

--b) agrandar: agrega 2 porciones al tamaño. 
--   En el caso de ya tener el máximo de porciones, las mismas siguen siendo dicho máximo

agrandar pizza = pizza {tamanio = min 10 $ tamanio pizza + 2}

--c) mezcladita: es la combinación de 2 gustos de pizza, donde ocurre que los ingredientes se suman 
--   (sacando los repetidos) y las calorías se le suma la mitad de la primera pizza a combinar. 
--   Por ejemplo, si mezclamos una pizza de provolone con jamón con una napolitana, 
--   queda una napolitana con provolone y jamón. (Sí, este punto se pensó con hambre).

sinRepetidos [] = []
sinReptetidos (x:xs) = x : sinRepetidos (filter (/= x) xs)

mezcladita pizza pizza2 = sumarCalorias caloriasASumar pizza2 {
    ingredientes = sinRepetidos $ ingredientes pizza2 ++ ingredientes pizza
}
    where caloriasASumar = (/2) . calorias $ pizza

sumarCalorias cal pizza = pizza {calorias = calorias pizza + cal}

type Pedido = [Pizza]

--5) Calcular el nivel de satisfacción de un pedido, que es la sumatoria de la satisfacción 
--   que brinda cada pizza que compone el mismo. Nota: Usar composición.

satisfaccionPedido pizzas = sum $ map nivelSatisfaccion pizzas
satisfaccionPedido2 pizzas = foldl1 (+) (map nivelSatisfaccion pizzas)

satisfaccionPedido3 pizzas = sum . map nivelSatisfaccion $ pizzas
satisfaccionPedido4  = sum . map nivelSatisfaccion 

--6) 
--a pizzeriaLosHijosDePato: A cada pizza del pedido le agrega palmito. 
--  ¿Por qué?... No hay “por qué”... Sólo que son unos verdaderos hijos de Pato.

pizzeriaLosHijosDePato pizzas = map (nuevoIngrediente "palmito") pizzas
pizzeriaLosHijosDePato2 = map (nuevoIngrediente "palmito")

--b) pizzeriaElResumen: Dado un pedido, entrega las combinaciones de una pizza con la siguiente. 
--   Es decir, la primera con la segunda, la segunda con la tercera, etc. 
--   (y, por lo tanto, termina enviando un pedido que tiene una pizza menos que el pedido original, 
--   por el resultado de la combinación de pares de pizzas). Si el pedido tiene una sola pizza, no produce cambios. 
--   Nota: En esta definición puede usarse recursividad, aunque no es necesario. Pro-tip: función zip o zipWith.

pizzeriaElResumen pizzas 
    | length pizzas > 1 = map (\(p1,p2) -> mezcladita p1 p2) $ zip pizzas (tail pizzas)
    | otherwise = pizzas

pizzeriaElResumen2 pizzas 
    | length pizzas > 1 = zipWith mezcladita pizzas (tail pizzas)
    | otherwise = pizzas

--c) pizzeriaEspecial: Una pizzería especial tiene un sabor predilecto de pizza y 
--   todas las pizzas del pedido las combina con esa.
--   La pizzeriaPescadito es un caso particular de este, donde su sabor predilecto es 
--   anchoas básica: tiene salsa, anchoas, sólo posee 270 calorías y es de 8 porciones.
type Pizzeria = Pedido -> Pedido
type PizzeriaEspecial = Pizza -> Pizzeria

--pizzeriaEspecial predilecta pedidos = map (mezcladita predilecta) pedidos
pizzeriaEspecial predilecta = map (mezcladita predilecta)

pizzeriaPescadito :: Pizzeria
anchoasBasica = Pizza ["salsa", "anchoas"] 8 270

--pizzeriaPescadito pedidos = pizzeriaEspecial anchoasBasica pedidos
pizzeriaPescadito = pizzeriaEspecial anchoasBasica 

--d) pizzeriaGourmet: Del pedido solo envía aquellas para las cuales el nivel de satisfacción 
--   supera el nivel de exquisitez de la pizzería... el resto no, las considera deplorables. 
--   Y, de regalo, a aquellas que manda las agranda a la siguiente escala, si esto es posible.
--   La pizzeriaLaJauja, es un clásico caso gourmet con un parámetro de exquisitez de 399.

type PizzeriaGourmet = Number -> Pizzeria
pizzeriaGourmet exquisitez pedidos = map agrandar $ filter (\p-> nivelSatisfaccion p > exquisitez) pedidos

pizzeriaGourmet2 exquisitez  = map agrandar . filter ((>exquisitez).nivelSatisfaccion) 

pizzeriaLaJauja :: Pizzeria
pizzeriaLaJauja = pizzeriaGourmet 399 

--7) 
--a implementar la función sonDignasDeCalleCorrientes que, dado un pedido y una lista de pizzerías,
--  devuelve aquellas pizzerías que mejoran la satisfacción del pedido.

sonDignasDeCalleCorrientes pedido pizzerias = filter (esDignaCalleCorrientes pedido) pizzerias


esDignaCalleCorrientes :: Pedido -> Pizzeria -> Bool
esDignaCalleCorrientes  pedido pizzeria= satisfaccionPedido pedido < (satisfaccionPedido . pizzeria) pedido

--b) Dado un pedido y una lista de pizzerías encontrar la pizzería que maximiza la satisfacción que otorga el pedido.


maximaSatisfaccion pedido pizzerias = maximum . map nivelSatisfaccion $ map (pedido) pizzerias
maximaSatisfaccion2 pedido pizzerias = maximum . map (nivelSatisfaccion . pedido) $ pizzerias

maximaSatisfaccion3 pedido pizzerias = foldl1 (mejorPizzeria pedido) pizzerias

mejorPizzeria pedido pizzeria1 pizzeria2 
    | valor pizzeria1 > valor pizzeria2 = pizzeria1
    | otherwise = pizzeria2
    where valor pizzeria = nivelSatisfaccion . pizzeria $ pedido

--8) Explicar el tipo de la siguiente función:
--   yoPidoCualquierPizza x y z = any (odd . x . fst) z && all (y . snd) z
--   Se trata de una funcion Booleana, donde si cumple con ambos criterios la resolucion sera True
--   Por el lado derecho evalua que todos los segundos elemntos de z evaluados en y sean True
--   Por el lado izquierodo evalua si algun primer elemento de z, evaluado en x es impar

--9) Bonus:
--   Todos tenemos preferencias, y algunas veces nuestra preferencia es que se junten los mejores pizzeros 
--   y hagan lo que mejor saben. Implementar laPizzeriaPredilecta, que dada una lista de pizzerias, 
--   devuelve una pizzería teóricamente perfecta que haga los pedidos de todas juntas.

laPizzeriaPredilecta :: [Pizzeria] -> Pizzeria
laPizzeriaPredilecta pizzerias = foldl1 (.) pizzerias

