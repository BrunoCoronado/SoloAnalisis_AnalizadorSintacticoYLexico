
Public Class Comentario
    Private _nombre As String
    Private _texto As String

    Public Property nombre As String
        Get
            Return Me._nombre
        End Get
        Set(value As String)
            Me._nombre = value
        End Set
    End Property

    Public Property texto As String
        Get
            Return Me._texto
        End Get
        Set(value As String)
            Me._texto = value
        End Set
    End Property
End Class
