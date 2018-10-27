Public Class AnalizadorCentral
    Private lexico As AnalizadorLexico
    Private sintactico As AnalizadorSintactico

    Public Sub New()
        lexico = New AnalizadorLexico
        sintactico = New AnalizadorSintactico
    End Sub

    Public Sub analizarLexico(entrada As ArrayList)
        Dim salidaLexico = lexico.analizar(entrada)
        Dim salidaSintactico = sintactico.analizar(salidaLexico("tokens"))
        If salidaLexico.ContainsKey("errores") Then
            If salidaSintactico.ContainsKey("errores") Then
                MessageBox.Show("Errores en el codigo!", "ERROR")
            Else
                MessageBox.Show("Errores en el codigo!", "ERROR")
            End If
        ElseIf salidaSintactico.ContainsKey("errores") Then
            MessageBox.Show("Errores en el codigo!", "ERROR")
        Else
            MessageBox.Show("Sin Errores en el codigo!", "SIN ERRORES")
        End If

    End Sub

    Public Sub reportarAnalisis(entrada As ArrayList)
        Dim graficador As New Graficador
        Dim salidaLexico = lexico.analizar(entrada)
        Dim salidaSintactico = sintactico.analizar(salidaLexico("tokens"))
        If salidaLexico.ContainsKey("errores") Then
            If salidaSintactico.ContainsKey("errores") Then
                graficador.dibujarReporteErrores(anidarErrores(salidaLexico("errores"), salidaSintactico("errores")))
            Else
                graficador.dibujarReporteErrores(salidaLexico("errores"))
            End If
        ElseIf salidaSintactico.ContainsKey("errores") Then
            graficador.dibujarReporteErrores(salidaSintactico("errores"))
        Else
            graficador.dibujarReporteTokens(salidaLexico("tokens"))
        End If
    End Sub

    Public Sub diagramarCodigo(entrada As ArrayList)
        Dim graficador As New Graficador
        Dim salidaLexico = lexico.analizar(entrada)
        Dim salidaSintactico = sintactico.analizar(salidaLexico("tokens"))
        If salidaLexico.ContainsKey("errores") Then
            If salidaSintactico.ContainsKey("errores") Then
                graficador.dibujarReporteErrores(anidarErrores(salidaLexico("errores"), salidaSintactico("errores")))
            Else
                graficador.dibujarReporteErrores(salidaLexico("errores"))
            End If
        ElseIf salidaSintactico.ContainsKey("errores") Then
            graficador.dibujarReporteErrores(salidaSintactico("errores"))
        Else
            graficador.dibujarDiagrama(validarExistencias(sintactico.obtenerEstructuras()))
        End If
    End Sub

    Private Function validarExistencias(estructuras As Dictionary(Of String, ArrayList)) As Dictionary(Of String, ArrayList)
        Dim clases As New ArrayList
        Dim comentarios As New ArrayList
        Dim asociaciones As New ArrayList
        If estructuras.ContainsKey("clases") Then
            clases = estructuras("clases")
        End If
        If estructuras.ContainsKey("comentarios") Then
            comentarios = estructuras("comentarios")
        End If
        If estructuras.ContainsKey("asociaciones") Then
            asociaciones = estructuras("asociaciones")
        End If
        Dim padres As New ArrayList
        Dim hijos As New ArrayList
        For Each asociacion As Asociacion In asociaciones
            padres.Add(asociacion.padre)
            hijos.Add(asociacion.hijo)
        Next

        For Each padre As String In padres
            Dim padreEncontrado As Boolean
            For Each clase As Clase In clases
                If clase.nombre.Equals(padre) Then
                    padreEncontrado = True
                    Exit For
                End If
            Next
            If Not padreEncontrado Then
                For Each comentario As Comentario In comentarios
                    If comentario.nombre.Equals(padre) Then
                        padreEncontrado = True
                        Exit For
                    End If
                Next
                If Not padreEncontrado Then
                    asociaciones = removerAsociacion(padre, 0, asociaciones)
                End If
            End If
        Next

        For Each hijo As String In hijos
            Dim hijoEncontrado As Boolean
            For Each clase As Clase In clases
                If clase.nombre.Equals(hijo) Then
                    hijoEncontrado = True
                    Exit For
                End If
            Next
            If Not hijoEncontrado Then
                For Each comentario As Comentario In comentarios
                    If comentario.nombre.Equals(hijo) Then
                        hijoEncontrado = True
                        Exit For
                    End If
                Next
                If Not hijoEncontrado Then
                    asociaciones = removerAsociacion(hijo, 1, asociaciones)
                End If
            End If
        Next
        estructuras("asociaciones") = asociaciones
        Return estructuras
    End Function

    Private Function removerAsociacion(valor As String, tipo As Integer, asociaciones As ArrayList) As ArrayList
        For Each asociacion As Asociacion In asociaciones
            If tipo = 0 Then
                If asociacion.padre.Equals(valor) Then
                    asociaciones.Remove(asociacion)
                    Return asociaciones
                End If
            Else
                If asociacion.hijo.Equals(valor) Then
                    asociaciones.Remove(asociacion)
                    Return asociaciones
                End If
            End If
        Next
        Return asociaciones
    End Function

    Private Function anidarErrores(lexicos As ArrayList, sintacticos As ArrayList) As ArrayList
        For Each er As Token In sintacticos
            lexicos.Add(er)
        Next
        Return lexicos
    End Function
End Class
