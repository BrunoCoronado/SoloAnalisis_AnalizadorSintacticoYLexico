Public Class AnalizadorLexico
    Private tokens As ArrayList
    Private palabra As String
    Private textoEntreComillas As String
    Private inicioTextoEntreComillas As Integer
    Private codigo As String
    'banderas para el analisis
    Dim esIdentificador As Boolean
    Dim identificadorConDigitos As Boolean
    Dim esNumero As Boolean
    Dim comillasAbiertas As Boolean

    Public Sub New()
        tokens = New ArrayList()
        palabra = ""
        codigo = ""
    End Sub

    Public Sub analizar(entrada As ArrayList)
        Dim noLinea As Integer
        Dim indiceCadena As Integer
        For noLinea = 0 To (entrada.Count - 1)
            For indiceCadena = 0 To (entrada.Item(noLinea).Length - 1)
                Dim caracter As String = entrada.Item(noLinea)(indiceCadena).ToString.ToLower
                If comillasAbiertas Then
                    If caracter.Equals("""") Then
                        comillasAbiertas = False
                        agregarTokenATabla(textoEntreComillas, "Texto Comnetario", 300, inicioTextoEntreComillas, noLinea)
                        agregarTokenATabla(caracter, "Comillas", 260, indiceCadena, noLinea)
                    Else
                        textoEntreComillas = textoEntreComillas + caracter
                    End If
                Else
                    Select Case caracter
                        Case vbCrLf
                            validarEstadoPalabra((indiceCadena - palabra.Length), noLinea)
                            codigo += caracter
                        Case vbCr
                            validarEstadoPalabra((indiceCadena - palabra.Length), noLinea)
                            codigo += caracter
                        Case vbTab
                            codigo += caracter
                        Case vbLf
                            validarEstadoPalabra((indiceCadena - palabra.Length), noLinea)
                            codigo += caracter
                        Case " "
                            codigo += caracter
                        Case "["
                            validarEstadoPalabra((indiceCadena - palabra.Length), noLinea)
                            codigo += (caracter.)
                            agregarTokenATabla(caracter, "Corchete Izquierda", 130, indiceCadena, noLinea)
                        Case "]"
                            validarEstadoPalabra((indiceCadena - palabra.Length), noLinea)
                            agregarTokenATabla(caracter, "Corchete Derecha", 140, indiceCadena, noLinea)
                        Case "{"
                            validarEstadoPalabra((indiceCadena - palabra.Length), noLinea)
                            agregarTokenATabla(caracter, "Llave Izquierda", 150, indiceCadena, noLinea)
                        Case "}"
                            validarEstadoPalabra((indiceCadena - palabra.Length), noLinea)
                            agregarTokenATabla(caracter, "Llave Derecha", 160, indiceCadena, noLinea)
                        Case "("
                            validarEstadoPalabra((indiceCadena - palabra.Length), noLinea)
                            agregarTokenATabla(caracter, "Parentesis Izquierda", 210, indiceCadena, noLinea)
                        Case ")"
                            validarEstadoPalabra((indiceCadena - palabra.Length), noLinea)
                            agregarTokenATabla(caracter, "Parentesis Derecha", 220, indiceCadena, noLinea)
                        Case "="
                            validarEstadoPalabra((indiceCadena - palabra.Length), noLinea)
                            agregarTokenATabla(caracter, "Igual", 170, indiceCadena, noLinea)
                        Case "+"
                            validarEstadoPalabra((indiceCadena - palabra.Length), noLinea)
                            agregarTokenATabla(caracter, "Visibilidad Publico", 230, indiceCadena, noLinea)
                        Case "-"
                            validarEstadoPalabra((indiceCadena - palabra.Length), noLinea)
                            agregarTokenATabla(caracter, "Visibilidad Privado", 240, indiceCadena, noLinea)
                        Case "#"
                            validarEstadoPalabra((indiceCadena - palabra.Length), noLinea)
                            agregarTokenATabla(caracter, "Visibilidad Protegido", 250, indiceCadena, noLinea)
                        Case ":"
                            validarEstadoPalabra((indiceCadena - palabra.Length), noLinea)
                            agregarTokenATabla(caracter, "Dos Puntos", 200, indiceCadena, noLinea)
                        Case ";"
                            validarEstadoPalabra((indiceCadena - palabra.Length), noLinea)
                            agregarTokenATabla(caracter, "Punto y Coma", 180, indiceCadena, noLinea)
                        Case ","
                            validarEstadoPalabra((indiceCadena - palabra.Length), noLinea)
                            agregarTokenATabla(caracter, "Coma", 190, indiceCadena, noLinea)
                        Case "."
                            validarEstadoPalabra((indiceCadena - palabra.Length), noLinea)
                            agregarTokenATabla(caracter, "Punto", 270, indiceCadena, noLinea)
                        Case """"
                            comillasAbiertas = True
                            inicioTextoEntreComillas = indiceCadena + 1
                            validarEstadoPalabra((indiceCadena - palabra.Length), noLinea)
                            agregarTokenATabla(caracter, "Comillas", 260, indiceCadena, noLinea)
                        Case "a" To "z"
                            esIdentificador = True
                            palabra = palabra & caracter
                        Case "0" To "9"
                            If esIdentificador Or identificadorConDigitos Then
                                palabra = palabra & caracter
                                esIdentificador = False
                                identificadorConDigitos = True
                            Else
                                palabra = palabra & caracter
                                esNumero = True
                            End If
                        Case "_"
                            If esIdentificador Then
                                palabra = palabra & caracter
                            Else
                            End If
                        Case Else
                    End Select
                End If
            Next
        Next
        Dim graficador As New Graficador
        graficador.dibujarReporteTokens(tokens)
    End Sub

    Private Sub validarEstadoPalabra(ByVal columna As Integer, ByVal fila As Integer)
        If esIdentificador Then
            Dim tipo As String
            Dim token As Integer
            Select Case palabra
                Case "clase"
                    tipo = "Palabra Reservada"
                    token = 10
                Case "nombre"
                    tipo = "Palabra Reservada"
                    token = 20
                Case "atributos"
                    tipo = "Palabra Reservada"
                    token = 40
                Case "metodos"
                    tipo = "Palabra Reservada"
                    token = 60
                Case "asociacion"
                    tipo = "Palabra Reservada"
                    token = 310
                Case "agregacion"
                    tipo = "Palabra Reservada"
                    token = 80
                Case "composicion"
                    tipo = "Palabra Reservada"
                    token = 90
                Case "asociacionsimple"
                    tipo = "Palabra Reservada"
                    token = 110
                Case "herencia"
                    tipo = "Palabra Reservada"
                    token = 100
                Case "color"
                    tipo = "Palabra Reservada"
                    token = 30
                Case "comentario"
                    tipo = "Palabra Reservada"
                    token = 50
                Case "texto"
                    tipo = "Palabra Reservada"
                    token = 70
                Case "asociacionparacomentarios"
                    tipo = "Palabra Reservada"
                    token = 120
                Case Else
                    tipo = "Identificador"
                    token = 280
            End Select
            agregarTokenATabla(palabra, tipo, token, columna, fila)
            palabra = ""
            esIdentificador = False
        ElseIf identificadorConDigitos Then
            agregarTokenATabla(palabra, "Identificador", 280, columna, fila)
            palabra = ""
            identificadorConDigitos = False
        ElseIf esNumero Then
            agregarTokenATabla(palabra, "Numero", 290, columna, fila)
            palabra = ""
            esNumero = False
        End If
    End Sub

    Private Sub agregarTokenATabla(ByVal lexema As String, ByVal tipo As String, ByVal token As Integer, ByVal columna As Integer, ByVal fila As Integer)
        tokens.Add(New Token(lexema, tipo, token, columna, fila))
    End Sub

End Class
