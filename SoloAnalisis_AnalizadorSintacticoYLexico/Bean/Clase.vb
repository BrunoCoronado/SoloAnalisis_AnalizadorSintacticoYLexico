Public Class Clase
    Private _nombre As String
    Private _color As String
    Private _esInterfaz As Boolean = True
    Private _atributos As New ArrayList
    Private _metodos As ArrayList

    Public Property nombre As String
        Get
            Return Me._nombre
        End Get
        Set(value As String)
            Me._nombre = value
        End Set
    End Property

    Public Property color As String
        Get
            Return Me._color
        End Get
        Set(value As String)
            Me._color = value
        End Set
    End Property

    ReadOnly Property Interfaz As String
        Get
            Return Me._esInterfaz
        End Get
    End Property

    Public Sub setMetodo(metodo As Caracteristica)
        If _metodos Is Nothing Then
            _metodos = New ArrayList
        End If
        Me._metodos.Add(metodo)
    End Sub

    Public Function getMetodos() As ArrayList
        Return Me._metodos
    End Function

    Public Sub setAtributo(atributo As Caracteristica)
        Me._atributos.Add(atributo)
        _esInterfaz = False
    End Sub

    Public Function getAtributos() As ArrayList
        Return Me._atributos
    End Function
End Class
