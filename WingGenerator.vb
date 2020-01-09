'        00   00  
'    00           00
'  00      88888888888
' 00      8          00
' 0      88888        0
' 00    8            00
'  00  8            00
'    08           00
'        00   00
'Creator Farid Suleymanov
'Wing generator V0.1
'14.04.19
Public Class Form1
    Public Event UnhandledException(sender As Object, e As UnhandledExceptionEventArgs)

    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load

    End Sub

    Private Sub Button5_Click(sender As Object, e As EventArgs) Handles Button5.Click

        If TextBox1.Text = "" Then
            MessageBox.Show("Flow speed (m/s) field cannot be empty, fill in before proceeding",
                    "User Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
            TextBox1.Focus()
        ElseIf TextBox2.Text = "" Then
            MessageBox.Show("Plane weight (kg) filed cannot be empty, fill in before proceeding",
                    "User Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
            TextBox2.Focus()

        ElseIf TextBox3.Text = "" Then
            MessageBox.Show("Flight speed(m/s) field cannot be empty, fill in before proceeding",
                    "User Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
            TextBox3.Focus()

        ElseIf TextBox4.Text = "" Then
            MessageBox.Show("Air density(kg*s^2/m^4) field cannot be empty, fill in before proceeding",
                    "User Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
            TextBox4.Focus()

        ElseIf TextBox14.Text = "" Then
            MessageBox.Show("Engine trust(kg) field cannot be empty, fill in before proceeding",
                    "User Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
            TextBox14.Focus()

        ElseIf TextBox17.Text = "" Then
            MessageBox.Show("Enter File Name field cannot be empty, fill in before proceeding",
                    "User Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
            TextBox17.Focus()

        ElseIf TextBox18.Text = "" Then
            MessageBox.Show("Output File Path field cannot be empty, fill in before proceeding",
                    "User Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
            TextBox18.Focus()

        ElseIf TextBox10.Text = "" Then
            MessageBox.Show("Induction resistance coefficient filed cannot be empty, fill in before proceeding",
                    "User Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
            TextBox10.Focus()
        Else



            If (TextBox3.Text >= 40 And TextBox3.Text <= 281.26) Then
                TextBox15.Text = TextBox3.Text * 0.082898118212716573 'Angle of sweep'
                TextBox11.Text = TextBox15.Text * 0.25 'Wing dihedral angle'

            ElseIf (TextBox3.Text > 281.26 And TextBox3.Text <= 343) Then
                TextBox15.Text = ((TextBox3.Text - 239.1) * 0.87463556851311952) + 20 'Angle of sweep'
                TextBox11.Text = TextBox15.Text * 0.2 'Wing dihedral angle'

            ElseIf (TextBox3.Text > 343 And TextBox3.Text <= 1715) Then
                TextBox15.Text = 50 + (TextBox3.Text * 0.020824656393169511) 'Angle of sweep'
                TextBox11.Text = TextBox15.Text * (-1 * 0.125) 'Wing dihedral angle'

            End If

            If (TextBox3.Text >= 40 And TextBox3.Text <= 343) Then

                TextBox9.Text = (TextBox4.Text * (TextBox3.Text ^ 2)) / 2 'Fluid dynamic pressure(Pa)'

                TextBox13.Text = TextBox2.Text / (TextBox14.Text * TextBox4.Text * (TextBox1.Text ^ 2)) 'Front wing friction area(m^2)'

                TextBox12.Text = (TextBox2.Text / (TextBox9.Text * ((TextBox14.Text / (0.5 * ((TextBox3.Text) ^ 2) * TextBox4.Text)) / 2))) / ((TextBox10.Text * 2) * 3.1415926) 'Aspect ratio of wing extension'

                TextBox6.Text = Val(TextBox2.Text / (3.14 * TextBox9.Text * TextBox10.Text)) ^ (1 / 2) 'Wingspan(m)'

                TextBox7.Text = Val((TextBox9.Text * (((TextBox14.Text / (0.5 * ((TextBox3.Text) ^ 2) * TextBox4.Text)) / 2) ^ 2)) / (TextBox2.Text * TextBox10.Text * 3.1415926)) ^ (1 / 2) 'Central chord(m)'

                TextBox8.Text = (1 + (2 * TextBox7.Text)) / (2 * TextBox7.Text) 'Wing tip chord(m)'

                TextBox16.Text = (4 * ((TextBox7.Text) ^ 2)) / (1 + (2 * TextBox7.Text)) 'Wing root chord(m)'

                TextBox5.Text = (TextBox16.Text * TextBox8.Text * TextBox6.Text) / 2 'Wing area(m^2)'

                Dim p As Double = (TextBox6.Text * (Math.Tan(TextBox15.Text))) * 0.17191717088481606

                Dim k As Double = TextBox6.Text

                Dim j As Double = ((TextBox6.Text * (Math.Tan(TextBox15.Text))) * 0.17191717088481606) + TextBox8.Text

                Dim t As Double = TextBox16.Text

                Dim file1 As System.IO.StreamWriter

                file1 = My.Computer.FileSystem.OpenTextFileWriter(TextBox18.Text + "\" + TextBox17.Text + ".SCR", True)

                file1.WriteLine("Line")
                file1.WriteLine(0 & "," & 0)
                file1.WriteLine(p & "," & k)
                file1.WriteLine(j & "," & k)
                file1.WriteLine(t & "," & 0)
                file1.Close()

                ' In progress script for Fusion 360

                'Dim pp As Double = (TextBox6.Text * (Math.Tan(TextBox15.Text))) * 0.17191717088481606

                'Dim kk As Double = TextBox6.Text

                'Dim jj As Double = ((TextBox6.Text * (Math.Tan(TextBox15.Text))) * 0.17191717088481606) + TextBox8.Text

                'Dim tt As Double = TextBox16.Text

                'Dim file2 As System.IO.StreamWriter

                'file2 = My.Computer.FileSystem.OpenTextFileWriter(TextBox21.Text + "\" + TextBox18.Text + " ", True)


                'file2.WriteLine("Line")
                'file2.WriteLine(0 & "," & 0)
                'file2.WriteLine(pp & "," & kk)
                'file2.WriteLine(jj & "," & kk)
                'file2.WriteLine(tt & "," & 0)
                'file2.Close()

                TextBox19.Text = "AutoCAD and Fusion 360 script lockation" + TextBox17.Text

            End If
        End If

    End Sub

    Private Sub Button4_Click(sender As Object, e As EventArgs) Handles Button2.Click
        TextBox1.Text = ""
        TextBox2.Text = ""
        TextBox3.Text = ""
        TextBox4.Text = ""
        TextBox5.Text = ""
        TextBox6.Text = ""
        TextBox7.Text = ""
        TextBox8.Text = ""
        TextBox9.Text = ""
        TextBox10.Text = ""
        TextBox11.Text = ""
        TextBox12.Text = ""
        TextBox13.Text = ""
        TextBox14.Text = ""
        TextBox15.Text = ""
        TextBox16.Text = ""
    End Sub

    Private Sub TextBox1_KeyPress(sender As Object, e As KeyPressEventArgs) Handles TextBox1.KeyPress
        If (e.KeyChar < "0" OrElse e.KeyChar > "9") _
          AndAlso e.KeyChar <> ControlChars.Back AndAlso e.KeyChar <> "." Then
            e.Handled = True
        End If
    End Sub

    Private Sub TextBox2_KeyPress(sender As Object, e As KeyPressEventArgs) Handles TextBox2.KeyPress
        If (e.KeyChar < "0" OrElse e.KeyChar > "9") _
          AndAlso e.KeyChar <> ControlChars.Back AndAlso e.KeyChar <> "." Then
            e.Handled = True
        End If
    End Sub

    Private Sub TextBox3_KeyPress(sender As Object, e As KeyPressEventArgs) Handles TextBox3.KeyPress
        If (e.KeyChar < "0" OrElse e.KeyChar > "9") _
          AndAlso e.KeyChar <> ControlChars.Back AndAlso e.KeyChar <> "." Then
            e.Handled = True
        End If
    End Sub

    Private Sub TextBox4_KeyPress(sender As Object, e As KeyPressEventArgs) Handles TextBox4.KeyPress
        If (e.KeyChar < "0" OrElse e.KeyChar > "9") _
          AndAlso e.KeyChar <> ControlChars.Back AndAlso e.KeyChar <> "." Then
            e.Handled = True
        End If
    End Sub

    Private Sub TextBox10_KeyPress(sender As Object, e As KeyPressEventArgs) Handles TextBox10.KeyPress
        If (e.KeyChar < "0" OrElse e.KeyChar > "9") _
          AndAlso e.KeyChar <> ControlChars.Back AndAlso e.KeyChar <> "." Then
            e.Handled = True
        End If
    End Sub

    Private Sub TextBox14_KeyPress(sender As Object, e As KeyPressEventArgs) Handles TextBox14.KeyPress
        If (e.KeyChar < "0" OrElse e.KeyChar > "9") _
          AndAlso e.KeyChar <> ControlChars.Back AndAlso e.KeyChar <> "." Then
            e.Handled = True
        End If
    End Sub

    Private Sub TextBox18_KeyPress(sender As Object, e As KeyPressEventArgs)
        If (e.KeyChar <> "\" OrElse e.KeyChar <> "*") _
           Then
            e.Handled = True
        End If
    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        Label1.Text = "Скорость воздушного потока (м / с)"
        Label18.Text = "Корневая хорда крыла"
        Label4.Text = "Плотность воздуха (kg * s ^ 2 / m ^ 4)"
        Label3.Text = "Скорость полета (м / с)"
        Label5.Text = "Плошадь крыла (м ^ 2)"
        Label6.Text = "Размах крыла (м)"
        Label7.Text = "Вводные данные"
        Label8.Text = "Вывод"
        Label9.Text = "Центральная хорда крыла (м)"
        Label2.Text = "Вес самолета (кг)"
        Label19.Text = "Ввеедите название скрипта"
        Label17.Text = "Угол стреловидности"
        Label13.Text = "Угол V образности крыла"
        Label14.Text = "Удлинение крыла"
        Label15.Text = "Плошадь передней проекции крыла (м ^ 2)"
        Label16.Text = "Тяга двигателя"
        Label10.Text = "Концевая хорда крыла (м)"
        Label11.Text = "Динамическое давление потока (ПА)"
        Label12.Text = "Коеффициент сопротивления крыла"
        Label20.Text = "Путь сохранения скрипта"

    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        Label1.Text = "Predkosc powietrza (m / s)"
        Label18.Text = "Akord korzeniowy skrzydla (m)"
        Label4.Text = "Gestosc powietrza (kg * s ^ 2 / m ^ 4)"
        Label3.Text = "Predkosc lotu (m / s)"
        Label5.Text = "Obszar skrzydla (m ^ 2)"
        Label6.Text = "Rozpietosc skrzydla (m)"
        Label7.Text = "Wprowadzanie danych"
        Label8.Text = "Dane wyjsciowe"
        Label9.Text = "Centralny akord skrzydla (m)"
        Label2.Text = "Waga samolotu (kg)"
        Label19.Text = "Wprowadz Nazwe Skritu"
        Label17.Text = "Kat przemiatania"
        Label13.Text = "Kat skrzydla w ksztalcie litery V"
        Label14.Text = "Wydluzenie skrzydla"
        Label15.Text = "Obszar przedniej projekcji skrzydla (m ^ 2)"
        Label16.Text = "Ciag silnika (kg)"
        Label10.Text = "Cieciwa koncowa skrzydla (m)"
        Label11.Text = "Dynamiczne cisnienie plynu (Pa)"
        Label12.Text = "Indukcyjny wspólczynnik oporu"
        Label20.Text = "Wprowadz sciezke do pliku skryptu AutoCAD"
    End Sub

    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click
        Label1.Text = "Air speed (m / s)"
        Label18.Text = "Wing root chorde(m)"
        Label4.Text = "Air density(kg*s^2/m^4)"
        Label3.Text = "Flight speed (m / s)"
        Label5.Text = "Wing area(m^2)"
        Label6.Text = "Wingspan(m)"
        Label7.Text = "Input data"
        Label8.Text = "Output data"
        Label9.Text = "Central wing chord (m)"
        Label2.Text = "Weight of the aircraft (kg)"
        Label19.Text = "Enter Script Name"
        Label17.Text = "Angle of sweep"
        Label13.Text = "Wing dihedral angle"
        Label14.Text = "Aspect ratio of wing extension"
        Label15.Text = "Front wing friction area(m^2) (m ^ 2)"
        Label16.Text = "Engine thrust (kg)"
        Label10.Text = "Wing tip chorde(m)"
        Label11.Text = "Fluid dynamic pressure(Pa)"
        Label12.Text = "Induction ressistanse coefficient"
        Label20.Text = "Saved Script path. Like:C:\Users\username\Desktop"
    End Sub

End Class
