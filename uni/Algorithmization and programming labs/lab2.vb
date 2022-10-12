Public Class lab2
    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        Dim text As String
        text = "1.Даны два числа А и В. Если А>В, то А присвоить значение В, а В - значение А. Вывести прежние и полученные значения А и В."
        Dim a, b, container As Integer
        a = Val(InputBox("Please enter A "))
        b = Val(InputBox("Please enter B "))
        If a > b Then
            container = a
            a = b
            b = container
        End If
        MsgBox(text & vbNewLine & "A = " & a & vbNewLine & "B = " & b)
    End Sub

    Private Sub Button4_Click(sender As Object, e As EventArgs) Handles Button4.Click
        Dim text As String
        text = "2.Даны две пары чисел А, В и С, Д. Если А*В>С*Д, то вывести среднее арифметическое этих чисел, в противном случае среднее геометрическое."
        Dim a, b, c, d As Integer
        Dim gg As Decimal


        a = Val(InputBox("Please enter A "))
        b = Val(InputBox("Please enter B "))
        c = Val(InputBox("Please enter C "))
        d = Val(InputBox("Please enter D "))
        If (a * b) > (c * d) Then
            gg = ((a + b + c + d) / 4)
            MsgBox(text & vbNewLine & "среднее арифметическое этих чисел= " & gg)
        Else
            gg = ((a * b * c * d) ^ (1 / 4))
            MsgBox(text & vbNewLine & "среднее геометрическое этих чисел= " & gg)

        End If



    End Sub

    Private Sub Button6_Click(sender As Object, e As EventArgs) Handles Button6.Click
        Dim text As String
        text = "3.Даны три числа: a, b, c. Определить, можно ли построить треугольник, если интерпретировать a, b, c как длины сторон треугольника"
        Dim a, b, c As Decimal


        a = Val(InputBox("Please enter A "))
        b = Val(InputBox("Please enter B "))
        c = Val(InputBox("Please enter C "))
        If a > 0 And b > 0 And c > 0 Then
            MsgBox(text & vbNewLine & "можно построить треугольник ")
        Else
            MsgBox(text & vbNewLine & "не возможно построить треугольник ")

        End If
    End Sub

    Private Sub Button8_Click(sender As Object, e As EventArgs) Handles Button8.Click
        Dim text As String
        text = "Найти максимумы из 3, 4 и 5 чисел (одна процедура). Числа для сравнения вводить как с клавиатуры, так и с помощью функции RND (функция для получения случайного числа).
Функция RND возвращает случайное число в диапазоне от 0 до 1 с точностью 12 знаков после запятой. Для того, чтобы комфортно можно было использовать случайные числа в программах, следует умножать функцию на 10, 100, 1000 и т.д. Для отбрасывания дробной части используйте функции INT  или FIX. 
Например: x=Int(Rnd*100)
"
        Dim a, b, c As Decimal
        a = Int(Rnd() * 100)
        b = Int(Rnd() * 100)
        c = Int(Rnd() * 100)


        If (a < b) And (a < c) Then
            If (a < b) And (a < c) Then
                MsgBox(text & vbNewLine & "Number 1 = " & a & vbNewLine & "Number 2 = " & b & vbNewLine & "Number 3 = " & c & vbNewLine & "the max = " & c)
            Else
                MsgBox(text & vbNewLine & "Number 1 = " & a & vbNewLine & "Number 2 = " & b & vbNewLine & "Number 3 = " & c & vbNewLine & "the max = " & b)
            End If
        ElseIf (b < a) And (b < c) Then
            If a < c Then
                MsgBox(text & vbNewLine & "Number 1 = " & a & vbNewLine & "Number 2 = " & b & vbNewLine & "Number 3 = " & c & vbNewLine & "the max = " & c)
            Else
                MsgBox(text & vbNewLine & "Number 1 = " & a & vbNewLine & "Number 2 = " & b & vbNewLine & "Number 3 = " & c & vbNewLine & "the max = " & a)
            End If

        ElseIf (c < a) And (c < b) Then
            If a < b Then
                MsgBox(text & vbNewLine & "Number 1 = " & a & vbNewLine & "Number 2 = " & b & vbNewLine & "Number 3 = " & c & vbNewLine & "the max = " & b)
            Else
                MsgBox(text & vbNewLine & "Number 1 = " & a & vbNewLine & "Number 2 = " & b & vbNewLine & "Number 3 = " & c & vbNewLine & "the max = " & a)
            End If
        End If

    End Sub

    Private Sub Button10_Click(sender As Object, e As EventArgs) Handles Button10.Click
        Dim text As String
        text = "5. Даны три числа А, В, С.  Найти и вывести наименьшую по абсолютной величине (функция abs()) разность из трех возможных."
        Dim a, b, c
        Dim modl1, modl2, modl3

        a = Val(InputBox("Enter A"))
        b = Val(InputBox("Enter B"))
        c = Val(InputBox("Enter C"))
        modl1 = Math.Abs(a - b)
        modl2 = Math.Abs(a - c)
        modl3 = Math.Abs(c - b)
        If modl1 < modl2 And modl1 < modl3 Then
            MsgBox(text & vbNewLine & "вывести наименьшую по абсолютной  " & modl1)
        ElseIf modl2 < modl1 And modl2 < modl3 Then
            MsgBox(text & vbNewLine & "вывести наименьшую по абсолютной  " & modl2)
        Else
            MsgBox(text & vbNewLine & "вывести наименьшую по абсолютной  " & modl3)
        End If
    End Sub

    Private Sub Button12_Click(sender As Object, e As EventArgs) Handles Button12.Click
        Dim text As String
        text = "6.Написать программу, которая вычисляет по требованию периметр и площадь одной из шести фигур (треугольник, прямоугольник, квадрат, круг, трапеция, ромб). Оператор ВЫБОР использовать нельзя."
        Dim a, b, c, d, h, R As Decimal
        Dim shap As String
        shap = Val(InputBox("выведите номер из шести фигур " & a & vbNewLine & "1-треугольник" & a & vbNewLine & "2-прямоугольник" & a & vbNewLine & "3-квадрат" & a & vbNewLine & "4-круг" & a & vbNewLine & "5-трапеция" & a & vbNewLine & "6-ромб)"))

        If shap = "1" Then
            a = Val(InputBox("Enter A:"))
            b = Val(InputBox("Enter B:"))
            c = Val(InputBox("Enter c:"))
            h = Val(InputBox("Enter h:"))
            MsgBox(text & vbNewLine & "периметр  = " & a + b + c & vbNewLine & "площадь  = " & a * h / 2)



        ElseIf shap = "2" Then
            a = Val(InputBox("Enter A "))
            b = Val(InputBox("Enter B "))
            MsgBox(text & vbNewLine & "периметр = " & 2 * (a + b) & vbNewLine & "площадь = " & a * b)


        ElseIf shap = "3" Then
            a = Val(InputBox("Enter A"))
            MsgBox(text & vbNewLine & "периметр = " & a * 4 & vbNewLine & "площадь = " & a ^ 2)

        ElseIf shap = "4" Then
            R = Val(InputBox("Enter Radios:"))
            MsgBox(text & vbNewLine & "периметр = " & 2 * 3.14 * R & vbNewLine & "площадь = " & 3.14 * R ^ 2)


        ElseIf shap = "5" Then
            a = Val(InputBox("Enter основание A"))
            b = Val(InputBox("Enter основание B"))
            c = Val(InputBox("Enter C"))
            d = Val(InputBox("Enter D"))
            h = Val(InputBox("Enter height h"))
            MsgBox(text & vbNewLine & "периметр = " & a + b + c + d & vbNewLine & "площадь = " & (a + b) / 2 * h)

        ElseIf shap = "6" Then
            a = Val(InputBox("Enter A"))
            h = Val(InputBox("Enter  height h"))
            MsgBox(text & vbNewLine & "периметр = " & a * 4 & vbNewLine & "площадь = " & a * h)
        Else : MsgBox("неправильно фигур ")
        End If

    End Sub

    Private Sub Button14_Click(sender As Object, e As EventArgs) Handles Button14.Click
        Dim text As String
        text = "7-Написать программу, которая по номеру месяца (диапазон 1-12) определит время года и количество дней."
        Dim N As Integer
        N = Val(InputBox("Введите номер месяца:"))
        If N = 12 Or N = 1 Or N = 2 Then
            MsgBox(text & vbNewLine & " Зима.")
        ElseIf N <= 5 And N >= 3 Then
            MsgBox(text & vbNewLine & " Весна.")

        ElseIf N <= 8 And N >= 6 Then
            MsgBox(text & vbNewLine & " 'Лето.")
        ElseIf N <= 11 And N >= 9 Then
            MsgBox(text & vbNewLine & " 'Осень.")

        Else
            MsgBox(text & vbNewLine & "Нет такого месяца.")

        End If

    End Sub

    Private Sub Button16_Click(sender As Object, e As EventArgs) Handles Button16.Click
        Dim text As String
        text = "8. Даны координаты двух точек. Вычислить в какой координатной плоскости находятся данные точки – 1-4 четверть. Координаты точек отличны от нуля."
        Dim x, Y As Integer
        x = Val(InputBox("Введите x:"))
        Y = Val(InputBox("Введите y:"))
        If (x > 0) And (Y > 0) Then
            MsgBox(text & vbNewLine & ("the point " & x & "," & Y & ")") & vbNewLine & "Четверть I")
        ElseIf (x < 0) And (Y > 0) Then
            MsgBox(text & vbNewLine & ("the point " & x & "," & Y & ")") & vbNewLine & "Четверть II")
        ElseIf (x < 0) And (Y < 0) Then
            MsgBox(text & vbNewLine & ("the point (" & x & "," & Y & ")") & vbNewLine & "Четверть III")
        ElseIf (x > 0) And (Y < 0) Then
            MsgBox(text & vbNewLine & ("the point " & x & "," & Y & ")") & vbNewLine & "Четверть IV")
        Else
            MsgBox(text & vbNewLine & ("the point " & x & "," & Y & ")") & vbNewLine & "Точка лежит на оси")

        End If

    End Sub
End Class