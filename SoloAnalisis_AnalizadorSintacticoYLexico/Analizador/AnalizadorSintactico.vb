Public Class AnalizadorSintactico
    Private preanalisis As Token
    Private tokens As ArrayList
    Private errores As ArrayList
    Private numPreanalisis As Integer
    Private existeError As Boolean
    Private leyendoMetodos As Boolean
    Private errorEnEstructura As Boolean

    Private clase As Clase
    Private comentario As Comentario
    Private asociacion As Asociacion
    Private caracteristica As Caracteristica

    Private clases As ArrayList
    Private comentarios As ArrayList
    Private asociaciones As ArrayList

    Public Sub New()
        numPreanalisis = 0
        errores = New ArrayList
        clases = New ArrayList
        comentarios = New ArrayList
    End Sub

    Public Function obtenerEstructuras() As Dictionary(Of String, ArrayList)
        Dim estructuras As New Dictionary(Of String, ArrayList)
        If clases.Count > 0 Then
            estructuras.Add("clases", clases)
        End If
        If comentarios.Count > 0 Then
            estructuras.Add("comentarios", comentarios)
        End If
        If asociaciones.Count > 0 Then
            estructuras.Add("asociaciones", asociaciones)
        End If
        Return estructuras
    End Function

    Public Function analizar(entrada As ArrayList) As Dictionary(Of String, ArrayList)
        Dim resultado As New Dictionary(Of String, ArrayList)
        tokens = entrada
        tokens.Add(New Token("$", "aceptacion", 0, 0, 0))
        preanalisis = tokens(numPreanalisis)
        A()
        resultado.Add("tokens", tokens)
        If existeError Or errorEnEstructura Then
            resultado.Add("errores", errores)
        End If
        Return resultado
    End Function

    Private Sub A()
        If preanalisis.token = 130 Then
            match(130)
            B()
        Else
            match(0)
        End If
    End Sub
    Private Sub B()
        If preanalisis.token = 10 Then
            match(10)
            If Not existeError Then
                clase = New Clase
            End If
            match(140)
            C()
            A()
        ElseIf preanalisis.token = 50 Then
            match(50)
            If Not existeError Then
                comentario = New Comentario
            End If
            match(140)
            K()
            A()
        Else
            match(310)
            If Not existeError Then
                asociaciones = New ArrayList
            End If
            match(140)
            O()
            A()
        End If
    End Sub
    Private Sub C()
        match(150)
        D()
    End Sub
    Private Sub D()
        If preanalisis.token = 130 Then
            match(130)
            E()
        Else
            match(160)
            If Not existeError Then
                añadirClase(clase)
            End If
        End If
    End Sub
    Private Sub E()
        If preanalisis.token = 20 Then
            match(20)
            match(140)
            match(170)
            F()
            If Not existeError Then
                clase.nombre = CType(tokens(numPreanalisis - 1), Token).lexema
            End If
            match(180)
            D()
        ElseIf preanalisis.token = 30 Then
            Dim color As String = ""
            match(30)
            match(140)
            match(170)
            G()
            If Not existeError Then
                color = color + CType(tokens(numPreanalisis - 1), Token).lexema
            End If
            match(190)
            G()
            If Not existeError Then
                color = color + "," + CType(tokens(numPreanalisis - 1), Token).lexema
            End If
            match(190)
            G()
            If Not existeError Then
                color = color + "," + CType(tokens(numPreanalisis - 1), Token).lexema
            End If
            match(180)
            If Not existeError Then
                clase.color = color
            End If
            D()
        ElseIf preanalisis.token = 60 Then
            match(60)
            If Not existeError Then
                leyendoMetodos = True
            End If
            match(140)
            H()
            D()
        Else
            match(40)
            If Not existeError Then
                leyendoMetodos = False
            End If
            match(140)
            H()
            D()
        End If
    End Sub
    Private Sub F()
        match(280)
    End Sub
    Private Sub G()
        match(290)
    End Sub
    Private Sub H()
        match(150)
        I()
    End Sub
    Private Sub I()
        If preanalisis.token = 210 Then
            match(210)
            If Not existeError Then
                caracteristica = New Caracteristica
            End If
            J()
            match(220)
            F()
            If Not existeError Then
                caracteristica.identificador = CType(tokens(numPreanalisis - 1), Token).lexema
            End If
            If preanalisis.token = 200 Then
                match(200)
                F()
                If Not existeError Then
                    caracteristica.tipo = CType(tokens(numPreanalisis - 1), Token).lexema
                End If
                match(180)
                If Not existeError Then
                    If leyendoMetodos Then
                        If Not verificarDuplicidadMetodo(caracteristica.identificador) Then
                            clase.setMetodo(caracteristica)
                        End If
                    Else
                        If Not verificarDuplicidadAtributos(caracteristica.identificador) Then
                            clase.setAtributo(caracteristica)
                        End If
                    End If
                End If
                I()
            Else
                match(180)
                If Not existeError Then
                    If leyendoMetodos Then
                        clase.setMetodo(caracteristica)
                    Else
                        clase.setAtributo(caracteristica)
                    End If
                End If
                I()
            End If
        Else
            match(160)
        End If
    End Sub
    Private Sub J()
        If preanalisis.token = 230 Then
            match(230)
        ElseIf preanalisis.token = 240 Then
            match(240)
        Else
            match(250)
        End If
        If Not existeError Then
            caracteristica.visibilidad = CType(tokens(numPreanalisis - 1), Token).lexema
        End If
    End Sub
    Private Sub K()
        match(150)
        L()
    End Sub
    Private Sub L()
        If preanalisis.token = 130 Then
            match(130)
            M()
        Else
            match(160)
            If Not existeError Then
                añadirComentario(comentario)
            End If
        End If
    End Sub
    Private Sub M()
        If preanalisis.token = 20 Then
            match(20)
            match(140)
            match(170)
            F()
            If Not existeError Then
                comentario.nombre = CType(tokens(numPreanalisis - 1), Token).lexema
            End If
            match(180)
            L()
        Else
            match(70)
            match(140)
            match(170)
            N()
            match(180)
            L()
        End If
    End Sub
    Private Sub N()
        match(260)
        match(300)
        If Not existeError Then
            comentario.texto = CType(tokens(numPreanalisis - 1), Token).lexema
        End If
        match(260)
    End Sub
    Private Sub O()
        match(150)
        P()
    End Sub
    Private Sub P()
        If preanalisis.token = 280 Then
            F()
            If Not existeError Then
                asociacion = New Asociacion
                asociacion.padre = CType(tokens(numPreanalisis - 1), Token).lexema
            End If
            match(200)
            If preanalisis.token = 280 Then
                F()
                If Not existeError Then
                    asociacion.hijo = CType(tokens(numPreanalisis - 1), Token).lexema
                End If
                match(180)
                If Not existeError Then
                    asociaciones.Add(asociacion)
                End If
                P()
            Else
                Q()
                match(200)
                F()
                If Not existeError Then
                    asociacion.hijo = CType(tokens(numPreanalisis - 1), Token).lexema
                End If
                match(180)
                If Not existeError Then
                    asociaciones.Add(asociacion)
                End If
                P()
            End If
        Else
            match(160)
        End If
    End Sub
    Private Sub Q()
        If preanalisis.token = 80 Then
            match(80)
        ElseIf preanalisis.token = 90 Then
            match(90)
        ElseIf preanalisis.token = 100 Then
            match(100)
        Else
            match(110)
        End If
        If Not existeError Then
            asociacion.asociacion = CType(tokens(numPreanalisis - 1), Token).lexema
        End If
    End Sub

    Private Sub match(token As Integer)
        Try
            If token = preanalisis.token Then
                numPreanalisis += 1
                preanalisis = tokens(numPreanalisis)
            Else
                guardarError(token)
            End If
            If token = 0 And Not existeError Then
                Console.WriteLine("sintaxis Correcta")
            End If
        Catch ex As Exception
            If Not existeError Then
                Console.WriteLine("sintaxis Correcta")
            Else
                Console.WriteLine("sintaxis Incorrecta")
            End If
        End Try
    End Sub

    Private Function buscarToken(token As Integer) As Token
        For Each tk As Token In tokens
            If token = tk.token Then
                Return tk
            End If
        Next
        Return Nothing
    End Function

    Private Sub guardarError(token As Integer)
        Dim e As Token = preanalisis
        e.tipo = "se esperaba " + buscarLexema(token) + " antes de este simbolo"
        errores.Add(e)
        existeError = True
    End Sub

    Private Function buscarLexema(token As Integer) As String
        Select Case (token)
            Case 0
                Return "Aceptacion"
            Case 10
                Return "Clase"
            Case 20
                Return "Nombre"
            Case 30
                Return "Color"
            Case 40
                Return "Atributos"
            Case 50
                Return "Comentario"
            Case 60
                Return "Metodos"
            Case 70
                Return "Texto"
            Case 80
                Return "Agregacion"
            Case 90
                Return "Composicion"
            Case 100
                Return "Herencia"
            Case 110
                Return "AsociacionSimple"
            Case 130
                Return "["
            Case 140
                Return "]"
            Case 150
                Return "{"
            Case 160
                Return "}"
            Case 170
                Return "="
            Case 180
                Return ";"
            Case 190
                Return ","
            Case 200
                Return ":"
            Case 210
                Return "("
            Case 220
                Return ")"
            Case 230
                Return "+"
            Case 240
                Return "-"
            Case 250
                Return "#"
            Case 260
                Return """"
            Case 270
                Return "."
            Case 280
                Return "Identificador"
            Case 290
                Return "Numero"
            Case 300
                Return "Comentario"
            Case 310
                Return "Asociacion"
        End Select
        Return ""
    End Function

    Private Sub añadirClase(cl As Clase)
        If cl.nombre IsNot Nothing Then
            If Not verificarDuplicidadClase(cl.nombre) Then
                If cl.color IsNot Nothing Then
                    If cl.getMetodos IsNot Nothing Then
                        clases.Add(cl)
                    Else
                        errores.Add(New Token(cl.nombre, "clase sin metodos", -1, CType(tokens(numPreanalisis - 3), Token).columna, CType(tokens(numPreanalisis - 3), Token).fila))
                        errorEnEstructura = True
                    End If
                Else
                    errores.Add(New Token(cl.nombre, "clase sin color", -1, CType(tokens(numPreanalisis - 3), Token).columna, CType(tokens(numPreanalisis - 3), Token).fila))
                    errorEnEstructura = True
                End If
            Else
                errores.Add(New Token(cl.nombre, "clase duplicada", -1, CType(tokens(numPreanalisis - 3), Token).columna, CType(tokens(numPreanalisis - 3), Token).fila))
                errorEnEstructura = True
            End If
        Else
            errores.Add(New Token("ClaseX", "clase sin nombre", -1, CType(tokens(numPreanalisis - 3), Token).columna, CType(tokens(numPreanalisis - 3), Token).fila))
            errorEnEstructura = True
        End If
    End Sub

    Private Sub añadirComentario(cm As Comentario)
        If cm.nombre IsNot Nothing Then
            If Not verificarDuplicidadComentario(cm.nombre) Then
                If cm.texto IsNot Nothing Then
                    comentarios.Add(cm)
                Else
                    errores.Add(New Token(cm.nombre, "comentario sin texto", -1, CType(tokens(numPreanalisis - 3), Token).columna, CType(tokens(numPreanalisis - 3), Token).fila))
                    errorEnEstructura = True
                End If
            Else
                errores.Add(New Token(cm.nombre, "comentario duplicado", -1, CType(tokens(numPreanalisis - 3), Token).columna, CType(tokens(numPreanalisis - 3), Token).fila))
                errorEnEstructura = True
            End If
        Else
            errores.Add(New Token("ComentarioX", "comentario sin nombre", -1, CType(tokens(numPreanalisis - 3), Token).columna, CType(tokens(numPreanalisis - 3), Token).fila))
            errorEnEstructura = True
        End If
    End Sub

    Private Function verificarDuplicidadClase(nombre As String) As Boolean
        For Each clase As Clase In clases
            If clase.nombre.Equals(nombre) Then
                Return True
            End If
        Next
        Return False
    End Function

    Private Function verificarDuplicidadMetodo(nombre As String) As Boolean
        If clase.getMetodos IsNot Nothing Then
            For Each metodo As Caracteristica In clase.getMetodos
                If metodo.identificador.Equals(nombre) Then
                    errores.Add(New Token(clase.nombre, "metodo duplicado en bloque clase", -1, CType(tokens(numPreanalisis - 3), Token).columna, CType(tokens(numPreanalisis - 3), Token).fila))
                    errorEnEstructura = True
                    Return True
                End If
            Next
        End If
        Return False
    End Function

    Private Function verificarDuplicidadAtributos(nombre As String) As Boolean
        For Each atributo As Caracteristica In clase.getAtributos
            If atributo.identificador.Equals(nombre) Then
                errores.Add(New Token(clase.nombre, "atributo duplicado en bloque clase", -1, CType(tokens(numPreanalisis - 3), Token).columna, CType(tokens(numPreanalisis - 3), Token).fila))
                errorEnEstructura = True
                Return True
            End If
        Next
        Return False
    End Function

    Private Function verificarDuplicidadComentario(nombre As String) As Boolean
        For Each comentario As Comentario In comentarios
            If comentario.nombre.Equals(nombre) Then
                Return True
            End If
        Next
        Return False
    End Function
End Class