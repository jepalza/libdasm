
#Inclib "dasm"


Type Operand
    operType As Long       ' Operand type (register, memory, etc) - enum Operand
    reg As Long            ' Register (if any)
    basereg As Long        ' Base register (if any)
    indexreg As Long       ' Index register (if any)
    scale As Long          ' Scale (if any)
    dispbytes As Long      ' Displacement bytes (0 = no displacement)
    dispoffset As Long     ' Displacement value offset
    immbytes As Long       ' Immediate bytes (0 = no immediate)
    immoffset As Long      ' Immediate value offset
    sectionbytes As Long   ' Section prefix bytes (0 = no section prefix)
    section As Integer     ' Section prefix value
    displacement As Long   ' Displacement value
    immediate As Long      ' Immediate value
    flags As Long          ' Operand flags
End Type

Type INSTRUCTION
    length As Long          'Instruction length
    instType As Long        'Instruction type - enum Instruction
    addrMode As Long        'Addressing mode  - enum Mode  //OffsetOf i.mode = 8
    opcode As Byte          'Actual opcode
    modrm As Byte           'MODRM byte
    sib As Byte             'SIB byte
    extindex As Long        'Extension table index
    fpuindex As Long        'FPU table index
    dispbytes As Long       'Displacement bytes (0 = no displacement)
    immbytes As Long        'Immediate bytes (0 = no immediate)
    sectionbytes As Long    'Section prefix bytes (0 = no section prefix)
    op1 As Operand          'First operand (if any)
    op2 As Operand          'Second operand (if any)
    op3 As Operand          'Additional operand (if any)
    ptr As Long             'Pointer to instruction table
    flags As Long           'Instruction flags
End Type

Enum disasmMode
    MODE_32 = 0
    MODE_16 = 1
End Enum

Enum disasmFormat
    FORMAT_ATT = 0
    FORMAT_INTEL = 1
End Enum

' nota: STDCALL si usamos la DLL, CDECL si usamos la libreria .A
Declare function get_instruction cdecl Alias "get_instruction" ( _
	ByRef inst As INSTRUCTION, _
	ByVal src As Byte ptr, _
	ByVal m As disasmMode = MODE_32 _
	) As Long

Declare Function get_instruction_string Cdecl Alias "get_instruction_string" (_
    ByRef inst As INSTRUCTION, _
    ByVal fmt As disasmFormat, _
    ByVal offset As Long, _
    ByRef buf As byte , _
    ByVal bufLen As Long _
	) As Long



' carga un binario en memoria
Function load_bin(nombre As String, b() As UByte) As Integer
	Dim As String sa
	Dim As Integer reg
	reg=1
	sa=Space(1024)
	Print "leyendo fichero "
   Open nombre For Binary Access Read As 1
    ReDim b(Lof(1)+Len(sa))' reservo como minimo "len(sa)" de mas, para que no de error si leemos menos que "len(sa)"
   	While Not Eof(1)
   		Get #1,reg,sa
   		For f As Integer =0 To Len(sa)-1
   			b((reg-1)+f)=Asc(Mid(sa,f+1,1))
   		Next
   		'''If reg Mod 16384=0 Then Locate 1,17:Print reg;" de";Lof(1)
   		reg+=Len(sa)
   	Wend
   Close 1
   Print
   Return reg-1
End Function



' variables    
Dim f As Integer

Dim ins As INSTRUCTION
Dim op As Operand
Dim nb As Long ' numero de bytes leidos
Dim res As Long ' salida de datos

Dim salida(255) As Byte

Dim addrasm As ULong
Dim addrbin As ULong
Dim addrfin As ULong

Dim datos As integer

Dim sa As String
Dim sb As String
Dim sc As String
Dim sd As String

Dim a As Byte
Dim c As UByte



    ' direccion de inicio, donde comienza a guardar codigo (no confundir con direccion de inicio en el binario de entrada)
    addrasm = 0

	 ' direccion desde donde comenzar a desensablar el binario, donde comienza a desensamblar
    addrbin = 0
    
    ' direccion final del binario hasta donde desensamblar (-1 para llegar al final del binario)
    addrfin = -1
    
    
    ReDim Shared binario() As Byte
    Dim tam As Integer
    Dim As String sn=Command

    If sn="" Then Print "Falta nombre del fichero.":Sleep:End
    tam=load_bin(sn,binario())
    Print "Desensamblando"
    If addrfin=-1 Then addrfin=tam

    'For f=0 To 32:Print Hex(binario(f),2);" ";:Next:print

    Open "salida.asm" For Output As 1
    
    Do
        nb = get_instruction(ins, @binario(addrbin))
        
        ' si la salida del numero de bytes leidos es 0, podian ser datos no reconocidos
        If nb = 0 Then ' dato no reconocido
        	 If datos=0 Then Print #1,";";Hex(addrasm,8);": ";
        	 Print #1,Hex(binario(addrbin),2);" ";
        	 datos+=1
        	 If datos=32 then datos=0:Print #1,""
        	 nb=1 ' me salto el byte leido
        	 GoTo no
        EndIf
        If datos Then Print #1," (datos)":datos=0
        
        'Print "Direccion:";addrbin;" Leidos:";sz

        
        res = get_instruction_string(ins, FORMAT_INTEL, addrasm, salida(0), UBound(salida))
        If res = 0 Then Print "error 1":Sleep: Exit Do

		  ' direccion actual
        sa= Hex(addrasm,8)
        
        ' bytes leidos
        sb=""
         For f = 1 To nb
         	c=binario(addrbin+(f-1))
         	sb=sb+Hex(c,2)
         Next        
         ' ajusto el espacio ocupado para que quepan 12 bytes
         sb=left(sb+Space(12*2),12*2)+" "
        
        sc=""
        a=0
         For f = 0 To UBound(salida)
         	c=salida(f)
         	If c=0 Then Exit For
         	If c=32 Then a=1 ' en cuanto aparece un espacio, dejamos de convertir a mayusculas
         	If (c>=97 And c<=122) And a=0 Then c=c-32 ' convierto los comandos a mayusculas
         	sd=Chr(c)
         	sc=sc+sd
         	salida(f)=0
         Next

			sa=sa+": "+sb+sc
			'Print sa:sleep
			Print #1,sa
			If InStr(UCase(sa),"RET") Then Print #1,"" ' hueco en cada RET
			If InStr(UCase(sa),"JMP") Then Print #1,"" ' hueco en cada JMP

		no:
        addrbin = addrbin + nb
        addrasm = addrasm + nb

        If addrbin > addrfin Then Exit Do
        
    Loop While 1
    
    Close 1
    
    
Print "Fin..."
sleep
