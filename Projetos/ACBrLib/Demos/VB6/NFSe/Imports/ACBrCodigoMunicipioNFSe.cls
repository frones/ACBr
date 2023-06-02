VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "ACBrCodigoMunicipioNFSe"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Public Sub AdicionarItemComboBox(cmb As ComboBox, descricao As String, valor As String)
    cmbCidadeEmitente.AddItem descricao
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = valor
End Sub

Public Sub NenhumCidade(ByRef cmbCidadeEmitente As ComboBox)
    cmbCidadeEmitente.AddItem "Nenhuma Cidade Selecionada"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 0
End Sub

Public Sub Rondonia(ByRef cmbCidadeEmitente As ComboBox)
    cmbCidadeEmitente.AddItem "Ariquemes - RO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 1100023
    cmbCidadeEmitente.AddItem "Cabixi - RO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 1100031
    cmbCidadeEmitente.AddItem "Cacoal - RO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 1100049
    cmbCidadeEmitente.AddItem "Colorado do Oeste - RO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 1100064
    cmbCidadeEmitente.AddItem "Guajara-Mirim - RO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 1100106
    cmbCidadeEmitente.AddItem "Jaru - RO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 1100114
    cmbCidadeEmitente.AddItem "Ji-Parana - RO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 1100122
    cmbCidadeEmitente.AddItem "Machadinho do Oeste - RO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 1100130
    cmbCidadeEmitente.AddItem "Ouro Preto do Oeste - RO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 1100155
    cmbCidadeEmitente.AddItem "Pimenta Bueno - RO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 1100189
    cmbCidadeEmitente.AddItem "Porto Velho - RO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 1100205
    cmbCidadeEmitente.AddItem "Presidente Medici - RO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 1100254
    cmbCidadeEmitente.AddItem "Vilhena - RO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 1100304
    cmbCidadeEmitente.AddItem "Alto Paraiso - RO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 1100403
    cmbCidadeEmitente.AddItem "Buritis - RO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 1100452
    cmbCidadeEmitente.AddItem "Cujubim - RO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 1100940
    cmbCidadeEmitente.AddItem "Itapuã do Oeste - RO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 1101104
    cmbCidadeEmitente.AddItem "Vale do Anari - RO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 1101757
End Sub

Public Sub Acre(ByRef cmbCidadeEmitente As ComboBox)
    cmbCidadeEmitente.AddItem "Rio Branco - AC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 1200401
    cmbCidadeEmitente.AddItem "Rodrigues Alves - AC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 1200427
    cmbCidadeEmitente.AddItem "Senador Guiomard - AC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 1200450
    cmbCidadeEmitente.AddItem "Tarauaca - AC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 1200609
End Sub

Public Sub Amazonas(ByRef cmbCidadeEmitente As ComboBox)
    cmbCidadeEmitente.AddItem "Iranduba - AM"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 1301852
    cmbCidadeEmitente.AddItem "Manaus - AM"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 1302603
End Sub

Public Sub Roraima(ByRef cmbCidadeEmitente As ComboBox)
    cmbCidadeEmitente.AddItem "Boa Vista - RR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 1400100
    cmbCidadeEmitente.AddItem "Bonfim - RR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 1400159
    cmbCidadeEmitente.AddItem "Caracarai - RR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 1400209
End Sub

Public Sub Para(ByRef cmbCidadeEmitente As ComboBox)
    cmbCidadeEmitente.AddItem "Altamira - PA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 1500602
    cmbCidadeEmitente.AddItem "Ananindeua - PA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 1500800
    cmbCidadeEmitente.AddItem "Aurora do Para - PA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 1500958
    cmbCidadeEmitente.AddItem "Barcarena - PA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 1501303
    cmbCidadeEmitente.AddItem "Belem - PA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 1501402
    cmbCidadeEmitente.AddItem "Belterra - PA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 1501451
    cmbCidadeEmitente.AddItem "Conceicao do Araguaia - PA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 1502707
    cmbCidadeEmitente.AddItem "Itaituba - PA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 1503606
    cmbCidadeEmitente.AddItem "Maraba - PA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 1504208
    cmbCidadeEmitente.AddItem "Marituba - PA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 1504422
    cmbCidadeEmitente.AddItem "Novo Progresso - PA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 1505031
    cmbCidadeEmitente.AddItem "Oriximina - PA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 1505304
    cmbCidadeEmitente.AddItem "Parauapebas - PA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 1505536
    cmbCidadeEmitente.AddItem "Redencao - PA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 1506138
    cmbCidadeEmitente.AddItem "Rio Maria - PA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 1506161
    cmbCidadeEmitente.AddItem "Santarem - PA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 1506807
    cmbCidadeEmitente.AddItem "Santo Antonio do Taua - PA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 1507003
    cmbCidadeEmitente.AddItem "Sao Caetano de Odivelas - PA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 1507102
    cmbCidadeEmitente.AddItem "Sao Felix do Xingu - PA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 1507300
    cmbCidadeEmitente.AddItem "Tucuma - PA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 1508084
    cmbCidadeEmitente.AddItem "Xinguara - PA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 1508407
    cmbCidadeEmitente.AddItem "Macapa - AP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 1600303
End Sub

Public Sub Tocantins(ByRef cmbCidadeEmitente As ComboBox)
    cmbCidadeEmitente.AddItem "Araguaína - TO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 1702109
    cmbCidadeEmitente.AddItem "Colinas do Tocantins - TO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 1705508
    cmbCidadeEmitente.AddItem "Guarai - TO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 1709302
    cmbCidadeEmitente.AddItem "Gurupi - TO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 1709500
    cmbCidadeEmitente.AddItem "Lagoa da Confusao - TO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 1711902
    cmbCidadeEmitente.AddItem "Miranorte - TO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 1713304
    cmbCidadeEmitente.AddItem "Paraiso do Tocantins - TO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 1716109
    cmbCidadeEmitente.AddItem "Porto Nacional - TO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 1718204
    cmbCidadeEmitente.AddItem "Palmas - TO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 1721000
    cmbCidadeEmitente.AddItem "Xambioa - TO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 1722107
End Sub

Public Sub Maranhao(ByRef cmbCidadeEmitente As ComboBox)
    cmbCidadeEmitente.AddItem "Açailandia - MA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2100055
    cmbCidadeEmitente.AddItem "Alcantara - MA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2100204
    cmbCidadeEmitente.AddItem "Alto Parnaiba - MA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2100501
    cmbCidadeEmitente.AddItem "Bacabal - MA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2101202
    cmbCidadeEmitente.AddItem "Balsas - MA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2101400
    cmbCidadeEmitente.AddItem "Caxias - MA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2103000
    cmbCidadeEmitente.AddItem "Grajau - MA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2104800
    cmbCidadeEmitente.AddItem "Imperatriz - MA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2105302
    cmbCidadeEmitente.AddItem "Pedreiras - MA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2108207
    cmbCidadeEmitente.AddItem "Santa Luzia - MA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2110005
    cmbCidadeEmitente.AddItem "Sao Luis - MA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2111300
    cmbCidadeEmitente.AddItem "Timon - MA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2112209
End Sub

Public Sub Piaui(ByRef cmbCidadeEmitente As ComboBox)
    cmbCidadeEmitente.AddItem "Luís Correia - PI"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2205706
    cmbCidadeEmitente.AddItem "Oeiras - PI"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2207009
    cmbCidadeEmitente.AddItem "Parnaíba - PI"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2207702
    cmbCidadeEmitente.AddItem "Picos - PI"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2208007
    cmbCidadeEmitente.AddItem "Teresina - PI"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2211001
    cmbCidadeEmitente.AddItem "Urucui - PI"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2211209
    cmbCidadeEmitente.AddItem "Valenca do Piaui - PI"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2211308
End Sub

Public Sub Ceara(ByRef cmbCidadeEmitente As ComboBox)
    cmbCidadeEmitente.AddItem "Aquiraz - CE"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2301000
    cmbCidadeEmitente.AddItem "Aracati - CE"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2301109
    cmbCidadeEmitente.AddItem "Barbalha - CE"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2301901
    cmbCidadeEmitente.AddItem "Beberibe - CE"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2302206
    cmbCidadeEmitente.AddItem "Crateus - CE"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2304103
    cmbCidadeEmitente.AddItem "Crato - CE"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2304202
    cmbCidadeEmitente.AddItem "Eusebio - CE"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2304285
    cmbCidadeEmitente.AddItem "Fortaleza - CE"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2304400
    cmbCidadeEmitente.AddItem "Guaraciaba do Norte - CE"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2305001
    cmbCidadeEmitente.AddItem "Itaitinga - CE"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2306256
    cmbCidadeEmitente.AddItem "Jijoca de Jericoacoara - CE"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2307254
    cmbCidadeEmitente.AddItem "Juazeiro do Norte - CE"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2307304
    cmbCidadeEmitente.AddItem "Maracanau - CE"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2307650
    cmbCidadeEmitente.AddItem "Pindoretama - CE"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2310852
    cmbCidadeEmitente.AddItem "Quixeramobim - CE"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2311405
    cmbCidadeEmitente.AddItem "Sobral - CE"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2312908
    cmbCidadeEmitente.AddItem "Taua - CE"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2313302
End Sub

Public Sub RioGrandedoNorte(ByRef cmbCidadeEmitente As ComboBox)
    cmbCidadeEmitente.AddItem "Parnamirim - RN"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2403251
    cmbCidadeEmitente.AddItem "Macaiba - RN"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2407104
    cmbCidadeEmitente.AddItem "Mossoro - RN"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2408003
    cmbCidadeEmitente.AddItem "Natal - RN"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2408102
    cmbCidadeEmitente.AddItem "Rodolfo Fernandes - RN"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2411007
    cmbCidadeEmitente.AddItem "Tibau - RN"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2411056
    cmbCidadeEmitente.AddItem "Sao Goncalo do Amarante - RN"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2412005
End Sub

Public Sub Paraiba(ByRef cmbCidadeEmitente As ComboBox)
    cmbCidadeEmitente.AddItem "Cabedelo - PB"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2503209
    cmbCidadeEmitente.AddItem "Campina Grande - PB"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2504009
    cmbCidadeEmitente.AddItem "Conde - PB"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2504603
    cmbCidadeEmitente.AddItem "Joao Pessoa - PB"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2507507
    cmbCidadeEmitente.AddItem "Piloes - PB"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2511608
    cmbCidadeEmitente.AddItem "Santarem - PB"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2513653
End Sub

Public Sub Pernambuco(ByRef cmbCidadeEmitente As ComboBox)
    cmbCidadeEmitente.AddItem "Bezerros - PE"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2601904
    cmbCidadeEmitente.AddItem "Cabo de Santo Agostinho - PE"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2602902
    cmbCidadeEmitente.AddItem "Camaragibe - PE"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2603454
    cmbCidadeEmitente.AddItem "Caruaru - PE"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2604106
    cmbCidadeEmitente.AddItem "Goiana - PE"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2606200
    cmbCidadeEmitente.AddItem "Jaboatao dos Guararapes - PE"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2607901
    cmbCidadeEmitente.AddItem "Olinda - PE"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2609600
    cmbCidadeEmitente.AddItem "Paulista - PE"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2610707
    cmbCidadeEmitente.AddItem "Petrolina - PE"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2611101
    cmbCidadeEmitente.AddItem "Recife - PE"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2611606
    cmbCidadeEmitente.AddItem "Vitoria de Santo Antao - PE"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2616407
End Sub

Public Sub Alagoas(ByRef cmbCidadeEmitente As ComboBox)
    cmbCidadeEmitente.AddItem "Arapiraca - AL"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2700300
    cmbCidadeEmitente.AddItem "Delmiro Gouveia - AL"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2702405
    cmbCidadeEmitente.AddItem "Maceio - AL"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2704302
    cmbCidadeEmitente.AddItem "Marechal Deodoro - AL"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2704708
    cmbCidadeEmitente.AddItem "Santana do Ipanema - AL"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2708006
End Sub

Public Sub Sergipe(ByRef cmbCidadeEmitente As ComboBox)
    cmbCidadeEmitente.AddItem "Aracaju - SE"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2800308
    cmbCidadeEmitente.AddItem "Barra dos Coqueiros - SE"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2800605
    cmbCidadeEmitente.AddItem "Campo do Brito - SE"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2801009
    cmbCidadeEmitente.AddItem "Estancia - SE"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2802106
    cmbCidadeEmitente.AddItem "Itabaiana - SE"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2802908
    cmbCidadeEmitente.AddItem "Japaratuba - SE"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2803302
    cmbCidadeEmitente.AddItem "Lagarto - SE"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2803500
    cmbCidadeEmitente.AddItem "Nossa Senhora do Socorro - SE"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2804805
End Sub

Public Sub Bahia(ByRef cmbCidadeEmitente As ComboBox)
    cmbCidadeEmitente.AddItem "Alcobaca - BA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2900801
    cmbCidadeEmitente.AddItem "Amargosa - BA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2901007
    cmbCidadeEmitente.AddItem "Amelia Rodrigues - BA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2901106
    cmbCidadeEmitente.AddItem "Barra - BA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2902708
    cmbCidadeEmitente.AddItem "Barreiras - BA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2903201
    cmbCidadeEmitente.AddItem "Bom Jesus da Lapa - BA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2903904
    cmbCidadeEmitente.AddItem "Camacan - BA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2905602
    cmbCidadeEmitente.AddItem "Camacari - BA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2905701
    cmbCidadeEmitente.AddItem "Campo Formoso - BA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2906006
    cmbCidadeEmitente.AddItem "Canavieiras - BA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2906303
    cmbCidadeEmitente.AddItem "Candeias - BA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2906501
    cmbCidadeEmitente.AddItem "Casa Nova - BA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2907202
    cmbCidadeEmitente.AddItem "Catu - BA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2907509
    cmbCidadeEmitente.AddItem "Correntina - BA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2909307
    cmbCidadeEmitente.AddItem "Cruz das Almas - BA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2909802
    cmbCidadeEmitente.AddItem "Dias D Avila - BA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2910057
    cmbCidadeEmitente.AddItem "Entre Rios - BA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2910503
    cmbCidadeEmitente.AddItem "Esplanada - BA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2910602
    cmbCidadeEmitente.AddItem "Eunapolis - BA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2910727
    cmbCidadeEmitente.AddItem "Feira de Santana - BA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2910800
    cmbCidadeEmitente.AddItem "Guanambi - BA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2911709
    cmbCidadeEmitente.AddItem "Ibotirama - BA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2913200
    cmbCidadeEmitente.AddItem "Ilheus - BA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2913606
    cmbCidadeEmitente.AddItem "Ipiau - BA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2913903
    cmbCidadeEmitente.AddItem "Irara - BA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2914505
    cmbCidadeEmitente.AddItem "Irece - BA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2914604
    cmbCidadeEmitente.AddItem "Itabela - BA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2914653
    cmbCidadeEmitente.AddItem "Itaberaba - BA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2914703
    cmbCidadeEmitente.AddItem "Itabuna - BA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2914802
    cmbCidadeEmitente.AddItem "Itapetinga - BA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2916401
    cmbCidadeEmitente.AddItem "Itiuba - BA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2917003
    cmbCidadeEmitente.AddItem "Jacobina - BA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2917508
    cmbCidadeEmitente.AddItem "Jaguarari - BA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2917706
    cmbCidadeEmitente.AddItem "Juazeiro - BA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2918407
    cmbCidadeEmitente.AddItem "Lapao - BA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2919157
    cmbCidadeEmitente.AddItem "Lauro de Freitas - BA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2919207
    cmbCidadeEmitente.AddItem "Luiz Eduardo Magalhaes - BA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2919553
    cmbCidadeEmitente.AddItem "Madre de Deus - BA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2919926
    cmbCidadeEmitente.AddItem "Mucuri - BA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2922003
    cmbCidadeEmitente.AddItem "Nordestina - BA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2922656
    cmbCidadeEmitente.AddItem "Piritiba - BA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2924801
    cmbCidadeEmitente.AddItem "Pocoes - BA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2925105
    cmbCidadeEmitente.AddItem "Pojuca - BA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2925204
    cmbCidadeEmitente.AddItem "Porto Seguro - BA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2925303
    cmbCidadeEmitente.AddItem "Prado - BA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2925501
    cmbCidadeEmitente.AddItem "Salvador - BA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2927408
    cmbCidadeEmitente.AddItem "Santa Cruz Cabralia - BA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2927705
    cmbCidadeEmitente.AddItem "Santo Antonio de Jesus - BA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2928703
    cmbCidadeEmitente.AddItem "Sao Desiderio - BA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2928901
    cmbCidadeEmitente.AddItem "Sao Sebastiao do Passe - BA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2929503
    cmbCidadeEmitente.AddItem "Senhor do Bonfim - BA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2930105
    cmbCidadeEmitente.AddItem "Simoes Filho - BA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2930709
    cmbCidadeEmitente.AddItem "Teixeira de Freitas - BA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2931350
    cmbCidadeEmitente.AddItem "Una - BA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2932507
    cmbCidadeEmitente.AddItem "Urandi - BA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2932606
    cmbCidadeEmitente.AddItem "Utinga - BA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2932804
    cmbCidadeEmitente.AddItem "Vera Cruz - BA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2933208
    cmbCidadeEmitente.AddItem "Vitoria da Conquista - BA"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 2933307
End Sub

Public Sub MinasGerais(ByRef cmbCidadeEmitente As ComboBox)
    cmbCidadeEmitente.AddItem "Albertina - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3101409
    cmbCidadeEmitente.AddItem "Alem Paraiba - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3101508
    cmbCidadeEmitente.AddItem "Alpinopolis - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3101904
    cmbCidadeEmitente.AddItem "Andradas - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3102605
    cmbCidadeEmitente.AddItem "Antonio Carlos - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3102902
    cmbCidadeEmitente.AddItem "Aracuai - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3103405
    cmbCidadeEmitente.AddItem "Araguari - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3103504
    cmbCidadeEmitente.AddItem "Araxa - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3104007
    cmbCidadeEmitente.AddItem "Arcos - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3104205
    cmbCidadeEmitente.AddItem "Barbacena - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3105608
    cmbCidadeEmitente.AddItem "Barroso - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3105905
    cmbCidadeEmitente.AddItem "Belo Horizonte - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3106200
    cmbCidadeEmitente.AddItem "Belo Oriente - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3106309
    cmbCidadeEmitente.AddItem "Belo Vale - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3106408
    cmbCidadeEmitente.AddItem "Betim - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3106705
    cmbCidadeEmitente.AddItem "Boa Esperanca - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3107109
    cmbCidadeEmitente.AddItem "Bocaiuva - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3107307
    cmbCidadeEmitente.AddItem "Bom Despacho - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3107406
    cmbCidadeEmitente.AddItem "Brumadinho - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3109006
    cmbCidadeEmitente.AddItem "Cajuri - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3110202
    cmbCidadeEmitente.AddItem "Camacho - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3110400
    cmbCidadeEmitente.AddItem "Camanducaia - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3110509
    cmbCidadeEmitente.AddItem "Campestre - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3111002
    cmbCidadeEmitente.AddItem "Campina Verde - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3111101
    cmbCidadeEmitente.AddItem "Campo Belo - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3111200
    cmbCidadeEmitente.AddItem "Campos Altos - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3111507
    cmbCidadeEmitente.AddItem "Capim Branco - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3112505
    cmbCidadeEmitente.AddItem "Capinopolis - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3112604
    cmbCidadeEmitente.AddItem "Carangola - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3113305
    cmbCidadeEmitente.AddItem "Caratinga - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3113404
    cmbCidadeEmitente.AddItem "Carmo do Rio Claro - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3114402
    cmbCidadeEmitente.AddItem "Carrancas - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3114600
    cmbCidadeEmitente.AddItem "Casa Grande - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3114907
    cmbCidadeEmitente.AddItem "Conceicao da Barra de Minas - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3115201
    cmbCidadeEmitente.AddItem "Cataguases - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3115300
    cmbCidadeEmitente.AddItem "Claudio - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3116605
    cmbCidadeEmitente.AddItem "Comendador Gomes - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3116902
    cmbCidadeEmitente.AddItem "Conceicao do Mato Dentro - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3117504
    cmbCidadeEmitente.AddItem "Conceicao dos Ouros - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3117801
    cmbCidadeEmitente.AddItem "Confins - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3117876
    cmbCidadeEmitente.AddItem "Conselheiro Lafaiete - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3118304
    cmbCidadeEmitente.AddItem "Contagem - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3118601
    cmbCidadeEmitente.AddItem "Coromandel - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3119302
    cmbCidadeEmitente.AddItem "Coronel Fabriciano - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3119401
    cmbCidadeEmitente.AddItem "Coronel Xavier Chaves - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3119708
    cmbCidadeEmitente.AddItem "Curvelo - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3120904
    cmbCidadeEmitente.AddItem "Delfinopolis - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3121209
    cmbCidadeEmitente.AddItem "Delta - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3121258
    cmbCidadeEmitente.AddItem "Divinopolis - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3122306
    cmbCidadeEmitente.AddItem "Dores de Campos - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3123007
    cmbCidadeEmitente.AddItem "Dores do Indaia - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3123205
    cmbCidadeEmitente.AddItem "Dores do Turvo - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3123304
    cmbCidadeEmitente.AddItem "Entre Rios de Minas - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3123908
    cmbCidadeEmitente.AddItem "Ervalia - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3124005
    cmbCidadeEmitente.AddItem "Extrema - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3125101
    cmbCidadeEmitente.AddItem "Formiga - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3126109
    cmbCidadeEmitente.AddItem "Fronteira - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3127008
    cmbCidadeEmitente.AddItem "Frutal - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3127107
    cmbCidadeEmitente.AddItem "Funilandia - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3127206
    cmbCidadeEmitente.AddItem "Governador Valadares - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3127701
    cmbCidadeEmitente.AddItem "Guanhaes - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3128006
    cmbCidadeEmitente.AddItem "Guape - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3128105
    cmbCidadeEmitente.AddItem "Guaxupe - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3128709
    cmbCidadeEmitente.AddItem "Ibituruna - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3130002
    cmbCidadeEmitente.AddItem "Igarape - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3130101
    cmbCidadeEmitente.AddItem "Igaratinga - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3130200
    cmbCidadeEmitente.AddItem "Iguatama - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3130309
    cmbCidadeEmitente.AddItem "Ingai - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3130804
    cmbCidadeEmitente.AddItem "Ipatinga - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3131307
    cmbCidadeEmitente.AddItem "Itabira - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3131703
    cmbCidadeEmitente.AddItem "Itajuba - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3132404
    cmbCidadeEmitente.AddItem "Itaobim - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3133303
    cmbCidadeEmitente.AddItem "Itapeva - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3133600
    cmbCidadeEmitente.AddItem "Itatiaiucu - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3133709
    cmbCidadeEmitente.AddItem "ItaudeMinas - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3133758
    cmbCidadeEmitente.AddItem "Itauna - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3133808
    cmbCidadeEmitente.AddItem "Itaverava - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3133907
    cmbCidadeEmitente.AddItem "Ituiutaba - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3134202
    cmbCidadeEmitente.AddItem "Itumirim - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3134301
    cmbCidadeEmitente.AddItem "Iturama - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3134400
    cmbCidadeEmitente.AddItem "Itutinga - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3134509
    cmbCidadeEmitente.AddItem "Jaboticatubas - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3134608
    cmbCidadeEmitente.AddItem "Jaiba - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3135050
    cmbCidadeEmitente.AddItem "Janauba - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3135100
    cmbCidadeEmitente.AddItem "Januaria - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3135209
    cmbCidadeEmitente.AddItem "Jeceaba - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3135407
    cmbCidadeEmitente.AddItem "Joao Monlevade - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3136207
    cmbCidadeEmitente.AddItem "Juiz de Fora - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3136702
    cmbCidadeEmitente.AddItem "Lagoa da Prata - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3137205
    cmbCidadeEmitente.AddItem "Lagoa Dourada - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3137403
    cmbCidadeEmitente.AddItem "Lagoa Santa - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3137601
    cmbCidadeEmitente.AddItem "Lavras - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3138203
    cmbCidadeEmitente.AddItem "Leopoldina - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3138401
    cmbCidadeEmitente.AddItem "Luminarias - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3138708
    cmbCidadeEmitente.AddItem "Luz - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3138807
    cmbCidadeEmitente.AddItem "Manhuacu - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3139409
    cmbCidadeEmitente.AddItem "Manhumirim - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3139508
    cmbCidadeEmitente.AddItem "Mantena - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3139607
    cmbCidadeEmitente.AddItem "Mar de Espanha - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3139805
    cmbCidadeEmitente.AddItem "Maria da Fe - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3139904
    cmbCidadeEmitente.AddItem "Mariana - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3140001
    cmbCidadeEmitente.AddItem "Martinho Campos - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3140506
    cmbCidadeEmitente.AddItem "Matias Barbosa - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3140803
    cmbCidadeEmitente.AddItem "Matozinhos - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3141108
    cmbCidadeEmitente.AddItem "Monte Carmelo - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3143104
    cmbCidadeEmitente.AddItem "Montes Claros - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3143302
    cmbCidadeEmitente.AddItem "Monte Siao - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3143401
    cmbCidadeEmitente.AddItem "Muriae - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3143906
    cmbCidadeEmitente.AddItem "Nazareno - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3144508
    cmbCidadeEmitente.AddItem "Nepomuceno - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3144607
    cmbCidadeEmitente.AddItem "Nova Lima - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3144805
    cmbCidadeEmitente.AddItem "Nova Serrana - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3145208
    cmbCidadeEmitente.AddItem "Ouro Fino - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3146008
    cmbCidadeEmitente.AddItem "Ouro Preto - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3146107
    cmbCidadeEmitente.AddItem "Padre Paraiso - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3146305
    cmbCidadeEmitente.AddItem "Paracatu - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3147006
    cmbCidadeEmitente.AddItem "Para de Minas - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3147105
    cmbCidadeEmitente.AddItem "Passos - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3147907
    cmbCidadeEmitente.AddItem "Patos de Minas - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3148004
    cmbCidadeEmitente.AddItem "Patrocinio - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3148103
    cmbCidadeEmitente.AddItem "Pedro Leopoldo - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3149309
    cmbCidadeEmitente.AddItem "Pequi - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3149606
    cmbCidadeEmitente.AddItem "Perdoes - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3149903
    cmbCidadeEmitente.AddItem "Pirajuba - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3150703
    cmbCidadeEmitente.AddItem "Pirapetinga - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3151107
    cmbCidadeEmitente.AddItem "Pirapora - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3151206
    cmbCidadeEmitente.AddItem "Piumhi - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3151503
    cmbCidadeEmitente.AddItem "Planura - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3151602
    cmbCidadeEmitente.AddItem "Poco Fundo - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3151701
    cmbCidadeEmitente.AddItem "Pocos de Caldas - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3151800
    cmbCidadeEmitente.AddItem "Pompeu - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3152006
    cmbCidadeEmitente.AddItem "Ponte Nova - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3152105
    cmbCidadeEmitente.AddItem "Porteirinha - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3152204
    cmbCidadeEmitente.AddItem "Porto Firme - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3152303
    cmbCidadeEmitente.AddItem "Pouso Alegre - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3152501
    cmbCidadeEmitente.AddItem "Pouso Alto - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3152600
    cmbCidadeEmitente.AddItem "Prados - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3152709
    cmbCidadeEmitente.AddItem "Prata - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3152808
    cmbCidadeEmitente.AddItem "Prudente de Morais - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3153608
    cmbCidadeEmitente.AddItem "Raposos - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3153905
    cmbCidadeEmitente.AddItem "Resende Costa - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3154200
    cmbCidadeEmitente.AddItem "Ribeirao das Neves - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3154606
    cmbCidadeEmitente.AddItem "Rio Acima - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3154804
    cmbCidadeEmitente.AddItem "Rio Piracicaba - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3155702
    cmbCidadeEmitente.AddItem "Ritapolis - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3156106
    cmbCidadeEmitente.AddItem "Sabara - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3156700
    cmbCidadeEmitente.AddItem "Sacramento - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3156908
    cmbCidadeEmitente.AddItem "Santa Barbara - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3157203
    cmbCidadeEmitente.AddItem "Santa Barbara do Tugurio - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3157302
    cmbCidadeEmitente.AddItem "Santa Cruz de Minas - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3157336
    cmbCidadeEmitente.AddItem "Santa Luzia - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3157807
    cmbCidadeEmitente.AddItem "Santana do Jacare - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3158805
    cmbCidadeEmitente.AddItem "Santana do Riacho - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3159001
    cmbCidadeEmitente.AddItem "Santana dos Montes - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3159100
    cmbCidadeEmitente.AddItem "Santa Rita do Sapucai - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3159605
    cmbCidadeEmitente.AddItem "Santa Vitoria - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3159803
    cmbCidadeEmitente.AddItem "Santo Antonio do Amparo - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3159902
    cmbCidadeEmitente.AddItem "Santo Antonio do Monte - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3160405
    cmbCidadeEmitente.AddItem "Santos Dumont - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3160702
    cmbCidadeEmitente.AddItem "Sao Bras do Suacui - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3160900
    cmbCidadeEmitente.AddItem "Sao Domingos das Dores - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3160959
    cmbCidadeEmitente.AddItem "Sao Francisco - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3161106
    cmbCidadeEmitente.AddItem "Sao Geraldo - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3161502
    cmbCidadeEmitente.AddItem "Sao Goncalo do Para - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3161809
    cmbCidadeEmitente.AddItem "Sao Gotardo - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3162104
    cmbCidadeEmitente.AddItem "Sao Joao Del Rei - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3162500
    cmbCidadeEmitente.AddItem "Sao Joaquim de Bicas - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3162922
    cmbCidadeEmitente.AddItem "Sao Jose da Lapa - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3162955
    cmbCidadeEmitente.AddItem "Sao Lourenco - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3163706
    cmbCidadeEmitente.AddItem "Sao Sebastiao do Paraiso - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3164704
    cmbCidadeEmitente.AddItem "Sao Tiago - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3165008
    cmbCidadeEmitente.AddItem "Sao Vicente de Minas - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3165305
    cmbCidadeEmitente.AddItem "Sete Lagoas - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3167202
    cmbCidadeEmitente.AddItem "Silveirania - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3167301
    cmbCidadeEmitente.AddItem "Teofilo Otoni - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3168606
    cmbCidadeEmitente.AddItem "Tiradentes - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3168804
    cmbCidadeEmitente.AddItem "Tres Coracoes - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3169307
    cmbCidadeEmitente.AddItem "Tres Marias - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3169356
    cmbCidadeEmitente.AddItem "Tumiritinga - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3169505
    cmbCidadeEmitente.AddItem "Uba - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3169901
    cmbCidadeEmitente.AddItem "Uberaba - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3170107
    cmbCidadeEmitente.AddItem "Uberlandia - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3170206
    cmbCidadeEmitente.AddItem "Unai - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3170404
    cmbCidadeEmitente.AddItem "Varginha - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3170701
    cmbCidadeEmitente.AddItem "Varzea da Palma - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3170800
    cmbCidadeEmitente.AddItem "Vazante - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3171006
    cmbCidadeEmitente.AddItem "Verissimo - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3171105
    cmbCidadeEmitente.AddItem "Vespasiano - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3171204
    cmbCidadeEmitente.AddItem "Vicosa - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3171303
    cmbCidadeEmitente.AddItem "Visconde do Rio Branco - MG"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3172004
End Sub

Public Sub EspiritoSanto(ByRef cmbCidadeEmitente As ComboBox)
    cmbCidadeEmitente.AddItem "Afonso Claudio - ES"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3200102
    cmbCidadeEmitente.AddItem "Aguia Branca - ES"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3200136
    cmbCidadeEmitente.AddItem "Alfredo Chaves - ES"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3200300
    cmbCidadeEmitente.AddItem "Anchieta - ES"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3200409
    cmbCidadeEmitente.AddItem "Aracruz - ES"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3200607
    cmbCidadeEmitente.AddItem "Atilio Vivacqua - ES"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3200706
    cmbCidadeEmitente.AddItem "Cachoeiro do Itapemirim - ES"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3201209
    cmbCidadeEmitente.AddItem "Cariacica - ES"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3201308
    cmbCidadeEmitente.AddItem "Colatina - ES"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3201506
    cmbCidadeEmitente.AddItem "Domingos Martins - ES"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3201902
    cmbCidadeEmitente.AddItem "Guarapari - ES"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3202405
    cmbCidadeEmitente.AddItem "Ibatiba - ES"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3202454
    cmbCidadeEmitente.AddItem "Ibiracu - ES"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3202504
    cmbCidadeEmitente.AddItem "Ibitirama - ES"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3202553
    cmbCidadeEmitente.AddItem "Iconha - ES"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3202603
    cmbCidadeEmitente.AddItem "Iuna - ES"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3203007
    cmbCidadeEmitente.AddItem "Linhares - ES"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3203205
    cmbCidadeEmitente.AddItem "Marataizes - ES"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3203320
    cmbCidadeEmitente.AddItem "Marechal Floriano - ES"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3203346
    cmbCidadeEmitente.AddItem "Nova Venecia - ES"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3203908
    cmbCidadeEmitente.AddItem "Pinheiros - ES"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3204104
    cmbCidadeEmitente.AddItem "Santa Maria de Jetiba - ES"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3204559
    cmbCidadeEmitente.AddItem "Sao Gabriel da Palha - ES"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3204708
    cmbCidadeEmitente.AddItem "Sao Mateus - ES"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3204906
    cmbCidadeEmitente.AddItem "Serra - ES"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3205002
    cmbCidadeEmitente.AddItem "Vargem Alta - ES"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3205036
    cmbCidadeEmitente.AddItem "Venda Nova do Imigrante - ES"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3205069
    cmbCidadeEmitente.AddItem "Viana - ES"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3205101
    cmbCidadeEmitente.AddItem "Vila Velha - ES"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3205200
    cmbCidadeEmitente.AddItem "Vitoria - ES"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3205309
End Sub

Public Sub RiodeJaneiro(ByRef cmbCidadeEmitente As ComboBox)
    cmbCidadeEmitente.AddItem "Angra dos Reis - RJ"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3300100
    cmbCidadeEmitente.AddItem "Araruama - RJ"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3300209
    cmbCidadeEmitente.AddItem "Areal - RJ"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3300225
    cmbCidadeEmitente.AddItem "Armacao dos Buzios - RJ"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3300233
    cmbCidadeEmitente.AddItem "Barra Mansa - RJ"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3300407
    cmbCidadeEmitente.AddItem "Belford Roxo - RJ"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3300456
    cmbCidadeEmitente.AddItem "Cabo Frio - RJ"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3300704
    cmbCidadeEmitente.AddItem "Cachoeiras de Macacu - RJ"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3300803
    cmbCidadeEmitente.AddItem "Carapebus - RJ"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3300936
    cmbCidadeEmitente.AddItem "Comendador Levy Gasparian - RJ"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3300951
    cmbCidadeEmitente.AddItem "Campos Dos Goytacazes - RJ"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3301009
    cmbCidadeEmitente.AddItem "Carmo - RJ"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3301207
    cmbCidadeEmitente.AddItem "Casimiro de Abreu - RJ"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3301306
    cmbCidadeEmitente.AddItem "Conceicao de Macabu - RJ"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3301405
    cmbCidadeEmitente.AddItem "Cordeiro - RJ"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3301504
    cmbCidadeEmitente.AddItem "Duque de Caxias - RJ"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3301702
    cmbCidadeEmitente.AddItem "Engenheiro Paulo de Frontin - RJ"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3301801
    cmbCidadeEmitente.AddItem "Iguaba Grande - RJ"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3301876
    cmbCidadeEmitente.AddItem "Itaborai - RJ"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3301900
    cmbCidadeEmitente.AddItem "Itaguai - RJ"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3302007
    cmbCidadeEmitente.AddItem "Itaperuna - RJ"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3302205
    cmbCidadeEmitente.AddItem "Itatiaia - RJ"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3302254
    cmbCidadeEmitente.AddItem "Macae - RJ"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3302403
    cmbCidadeEmitente.AddItem "Mage - RJ"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3302502
    cmbCidadeEmitente.AddItem "Mangaratiba - RJ"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3302601
    cmbCidadeEmitente.AddItem "Marica - RJ"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3302700
    cmbCidadeEmitente.AddItem "Mendes - RJ"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3302809
    cmbCidadeEmitente.AddItem "Mesquita - RJ"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3302858
    cmbCidadeEmitente.AddItem "Miguel Pereira - RJ"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3302908
    cmbCidadeEmitente.AddItem "Niteroi - RJ"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3303302
    cmbCidadeEmitente.AddItem "Nova Friburgo - RJ"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3303401
    cmbCidadeEmitente.AddItem "Nova Iguacu - RJ"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3303500
    cmbCidadeEmitente.AddItem "Paracambi - RJ"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3303609
    cmbCidadeEmitente.AddItem "Parati - RJ"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3303807
    cmbCidadeEmitente.AddItem "Paty do Alferes - RJ"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3303856
    cmbCidadeEmitente.AddItem "Petropolis - RJ"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3303906
    cmbCidadeEmitente.AddItem "Pinheiral - RJ"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3303955
    cmbCidadeEmitente.AddItem "Piraí - RJ"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3304003
    cmbCidadeEmitente.AddItem "Porto Real - RJ"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3304110
    cmbCidadeEmitente.AddItem "Quatis - RJ"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3304128
    cmbCidadeEmitente.AddItem "Resende - RJ"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3304201
    cmbCidadeEmitente.AddItem "Rio Bonito - RJ"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3304300
    cmbCidadeEmitente.AddItem "Rio das Flores - RJ"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3304508
    cmbCidadeEmitente.AddItem "Rio das Ostras - RJ"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3304524
    cmbCidadeEmitente.AddItem "Rio de Janeiro - RJ"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3304557
    cmbCidadeEmitente.AddItem "Santa Maria Madalena - RJ"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3304607
    cmbCidadeEmitente.AddItem "Santo Antonio de Padua - RJ"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3304706
    cmbCidadeEmitente.AddItem "Sao Goncalo - RJ"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3304904
    cmbCidadeEmitente.AddItem "Sao Joao da Barra - RJ"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3305000
    cmbCidadeEmitente.AddItem "Sao Joao de Meriti - RJ"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3305158
    cmbCidadeEmitente.AddItem "Sao Jose do Vale do Rio Preto - RJ"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3305158
    cmbCidadeEmitente.AddItem "Sao Pedro da Aldeia - RJ"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3305208
    cmbCidadeEmitente.AddItem "Saquarema - RJ"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3305505
    cmbCidadeEmitente.AddItem "Silva Jardim - RJ"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3305604
    cmbCidadeEmitente.AddItem "Teresopolis - RJ"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3305802
    cmbCidadeEmitente.AddItem "Trajano de Morais - RJ"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3305901
    cmbCidadeEmitente.AddItem "Tres Rios - RJ"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3306008
    cmbCidadeEmitente.AddItem "Valenca - RJ"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3306107
    cmbCidadeEmitente.AddItem "Vassouras - RJ"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3306156
    cmbCidadeEmitente.AddItem "Volta Redonda - RJ"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3306206
End Sub

Public Sub SaoPaulo(ByRef cmbCidadeEmitente As ComboBox)
    cmbCidadeEmitente.AddItem "Adamantina - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3500105
    cmbCidadeEmitente.AddItem "Aguai - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3500303
    cmbCidadeEmitente.AddItem "Agudos - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3500709
    cmbCidadeEmitente.AddItem "Americana - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3501608
    cmbCidadeEmitente.AddItem "Americo de Campos - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3501806
    cmbCidadeEmitente.AddItem "Amparo - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3501905
    cmbCidadeEmitente.AddItem "Analandia - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3502002
    cmbCidadeEmitente.AddItem "Andradina - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3502101
    cmbCidadeEmitente.AddItem "Angatuba - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3502200
    cmbCidadeEmitente.AddItem "Aparecida - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3502507
    cmbCidadeEmitente.AddItem "Aracatuba - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3502804
    cmbCidadeEmitente.AddItem "Aracoiaba da Serra - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3502903
    cmbCidadeEmitente.AddItem "Araraquara - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3503208
    cmbCidadeEmitente.AddItem "Araras - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3503307
    cmbCidadeEmitente.AddItem "Ariranha - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3503703
    cmbCidadeEmitente.AddItem "Aruja - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3503901
    cmbCidadeEmitente.AddItem "Assis - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3504008
    cmbCidadeEmitente.AddItem "Atibaia - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3504107
    cmbCidadeEmitente.AddItem "Avare - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3504503
    cmbCidadeEmitente.AddItem "Bady Bassitt - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3504602
    cmbCidadeEmitente.AddItem "Balsamo - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3504800
    cmbCidadeEmitente.AddItem "Bariri - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3505203
    cmbCidadeEmitente.AddItem "Barretos - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3505500
    cmbCidadeEmitente.AddItem "Barrinha - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3505609
    cmbCidadeEmitente.AddItem "Barueri - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3505708
    cmbCidadeEmitente.AddItem "Batatais - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3505906
    cmbCidadeEmitente.AddItem "Bauru - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3506003
    cmbCidadeEmitente.AddItem "Bebedouro - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3506102
    cmbCidadeEmitente.AddItem "Bertioga - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3506359
    cmbCidadeEmitente.AddItem "Birigui - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3506508
    cmbCidadeEmitente.AddItem "Bofete - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3506904
    cmbCidadeEmitente.AddItem "Boituva - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3507001
    cmbCidadeEmitente.AddItem "Botucatu - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3507506
    cmbCidadeEmitente.AddItem "Braganca Paulista - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3507605
    cmbCidadeEmitente.AddItem "Brodowski - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3507803
    cmbCidadeEmitente.AddItem "Brotas - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3507902
    cmbCidadeEmitente.AddItem "Buritama - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3508108
    cmbCidadeEmitente.AddItem "Cabreuva - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3508405
    cmbCidadeEmitente.AddItem "Cachoeira Paulista - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3508603
    cmbCidadeEmitente.AddItem "Caieiras - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3509007
    cmbCidadeEmitente.AddItem "Cajamar - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3509205
    cmbCidadeEmitente.AddItem "Campinas - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3509502
    cmbCidadeEmitente.AddItem "Campo Limpo Paulista - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3509601
    cmbCidadeEmitente.AddItem "Candido Rodrigues - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3510104
    cmbCidadeEmitente.AddItem "Capao Bonito - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3510203
    cmbCidadeEmitente.AddItem "Capela do Alto - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3510302
    cmbCidadeEmitente.AddItem "Capivari - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3510401
    cmbCidadeEmitente.AddItem "Caraguatatuba - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3510500
    cmbCidadeEmitente.AddItem "Carapicuiba - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3510609
    cmbCidadeEmitente.AddItem "Casa Branca - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3510807
    cmbCidadeEmitente.AddItem "Catanduva - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3511102
    cmbCidadeEmitente.AddItem "Cedral - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3511300
    cmbCidadeEmitente.AddItem "Cerquilho - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3511508
    cmbCidadeEmitente.AddItem "Colina - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3512001
    cmbCidadeEmitente.AddItem "Conchal - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3512209
    cmbCidadeEmitente.AddItem "Cordeiropolis - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3512407
    cmbCidadeEmitente.AddItem "Coroados - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3512506
    cmbCidadeEmitente.AddItem "Corumbatai - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3512704
    cmbCidadeEmitente.AddItem "Cosmorama - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3512902
    cmbCidadeEmitente.AddItem "Cotia - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3513009
    cmbCidadeEmitente.AddItem "Cruzeiro - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3513405
    cmbCidadeEmitente.AddItem "Cubatao - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3513504
    cmbCidadeEmitente.AddItem "Cunha - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3513603
    cmbCidadeEmitente.AddItem "Diadema - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3513801
    cmbCidadeEmitente.AddItem "Dois Corregos - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3514106
    cmbCidadeEmitente.AddItem "Dourado - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3514304
    cmbCidadeEmitente.AddItem "Dracena - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3514403
    cmbCidadeEmitente.AddItem "Duartina - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3514502
    cmbCidadeEmitente.AddItem "Embu das Artes - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3515004
    cmbCidadeEmitente.AddItem "Embu-Guacu - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3515103
    cmbCidadeEmitente.AddItem "Engenheiro Coelho - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3515152
    cmbCidadeEmitente.AddItem "Espirito Santo do Pinhal - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3515186
    cmbCidadeEmitente.AddItem "Fernandopolis - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3515509
    cmbCidadeEmitente.AddItem "Ferraz de Vasconcelos - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3515707
    cmbCidadeEmitente.AddItem "Franca - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3516200
    cmbCidadeEmitente.AddItem "Garca - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3516705
    cmbCidadeEmitente.AddItem "Gaviao Peixoto - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3516853
    cmbCidadeEmitente.AddItem "Guaicara - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3517208
    cmbCidadeEmitente.AddItem "Guaira - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3517406
    cmbCidadeEmitente.AddItem "Guapiaçu - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3517505
    cmbCidadeEmitente.AddItem "Guararapes - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3518206
    cmbCidadeEmitente.AddItem "Guararema - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3518305
    cmbCidadeEmitente.AddItem "Guaratingueta - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3518404
    cmbCidadeEmitente.AddItem "Guaruja - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3518701
    cmbCidadeEmitente.AddItem "Guarulhos - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3518800
    cmbCidadeEmitente.AddItem "Guatapara - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3518859
    cmbCidadeEmitente.AddItem "Herculandia - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3519006
    cmbCidadeEmitente.AddItem "Hortolandia - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3519071
    cmbCidadeEmitente.AddItem "Iacanga - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3519105
    cmbCidadeEmitente.AddItem "Ibate - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3519303
    cmbCidadeEmitente.AddItem "Ibitinga - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3519600
    cmbCidadeEmitente.AddItem "Ibiuna - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3519709
    cmbCidadeEmitente.AddItem "Igaracu do Tiete - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3520004
    cmbCidadeEmitente.AddItem "Igarapava - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3520103
    cmbCidadeEmitente.AddItem "Indaiatuba - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3520509
    cmbCidadeEmitente.AddItem "Indiana - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3520608
    cmbCidadeEmitente.AddItem "Iracemapolis - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3521408
    cmbCidadeEmitente.AddItem "Itajobi - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3521903
    cmbCidadeEmitente.AddItem "Itanhaem - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3522109
    cmbCidadeEmitente.AddItem "Itapetininga - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3522307
    cmbCidadeEmitente.AddItem "Itapeva - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3522406
    cmbCidadeEmitente.AddItem "Itapevi - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3522505
    cmbCidadeEmitente.AddItem "Itapira - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3522604
    cmbCidadeEmitente.AddItem "Itapolis - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3522703
    cmbCidadeEmitente.AddItem "Itaquaquecetuba - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3523107
    cmbCidadeEmitente.AddItem "Itatiba - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3523404
    cmbCidadeEmitente.AddItem "Itatinga - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3523503
    cmbCidadeEmitente.AddItem "Itu - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3523909
    cmbCidadeEmitente.AddItem "Itupeva - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3524006
    cmbCidadeEmitente.AddItem "Jaboticabal - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3524303
    cmbCidadeEmitente.AddItem "Jacarei - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3524402
    cmbCidadeEmitente.AddItem "Jaci - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3524501
    cmbCidadeEmitente.AddItem "Jaguariuna - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3524709
    cmbCidadeEmitente.AddItem "Jales - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3524808
    cmbCidadeEmitente.AddItem "Jambeiro - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3524907
    cmbCidadeEmitente.AddItem "Jandira - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3525003
    cmbCidadeEmitente.AddItem "Jardinopolis - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3525102
    cmbCidadeEmitente.AddItem "Jau - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3525300
    cmbCidadeEmitente.AddItem "Joanopolis - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3525508
    cmbCidadeEmitente.AddItem "Jundiai - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3525904
    cmbCidadeEmitente.AddItem "Junqueiropolis - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3526001
    cmbCidadeEmitente.AddItem "Laranjal Paulista - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3526407
    cmbCidadeEmitente.AddItem "Leme - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3526704
    cmbCidadeEmitente.AddItem "Lencois Paulista - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3526803
    cmbCidadeEmitente.AddItem "Limeira - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3526902
    cmbCidadeEmitente.AddItem "Lorena - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3527207
    cmbCidadeEmitente.AddItem "Lucelia - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3527405
    cmbCidadeEmitente.AddItem "Luis Antonio - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3527603
    cmbCidadeEmitente.AddItem "Macatuba - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3528007
    cmbCidadeEmitente.AddItem "Mairipora - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3528502
    cmbCidadeEmitente.AddItem "Marilia - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3529005
    cmbCidadeEmitente.AddItem "Matao - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3529302
    cmbCidadeEmitente.AddItem "Maua - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3529401
    cmbCidadeEmitente.AddItem "Mineiros do Tiete - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3529807
    cmbCidadeEmitente.AddItem "Mirante do Paranapanema - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3530201
    cmbCidadeEmitente.AddItem "Mirassol - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3530300
    cmbCidadeEmitente.AddItem "Mococa - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3530508
    cmbCidadeEmitente.AddItem "Mogi das Cruzes - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3530607
    cmbCidadeEmitente.AddItem "Mogi Mirim - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3530805
    cmbCidadeEmitente.AddItem "Monte Alto - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3531308
    cmbCidadeEmitente.AddItem "Monte Aprazivel - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3531407
    cmbCidadeEmitente.AddItem "Monte Azul Paulista - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3531506
    cmbCidadeEmitente.AddItem "Monte Mor - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3531803
    cmbCidadeEmitente.AddItem "Morro Agudo - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3531902
    cmbCidadeEmitente.AddItem "Morungaba - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3532009
    cmbCidadeEmitente.AddItem "Nazare Paulista - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3532405
    cmbCidadeEmitente.AddItem "Nova Granada - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3533007
    cmbCidadeEmitente.AddItem "Nova Odessa - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3533403
    cmbCidadeEmitente.AddItem "Novo Horizonte - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3533502
    cmbCidadeEmitente.AddItem "Olimpia - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3533908
    cmbCidadeEmitente.AddItem "Orlandia - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3534302
    cmbCidadeEmitente.AddItem "Osasco - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3534401
    cmbCidadeEmitente.AddItem "Osvaldo Cruz - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3534609
    cmbCidadeEmitente.AddItem "Ourinhos - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3534708
    cmbCidadeEmitente.AddItem "Ouroeste - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3534757
    cmbCidadeEmitente.AddItem "Paulinia - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3536505
    cmbCidadeEmitente.AddItem "Pederneiras - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3536703
    cmbCidadeEmitente.AddItem "Pedreira - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3537107
    cmbCidadeEmitente.AddItem "Penapolis - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3537305
    cmbCidadeEmitente.AddItem "Pereira Barreto - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3537404
    cmbCidadeEmitente.AddItem "Pilar do Sul - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3537909
    cmbCidadeEmitente.AddItem "Pindamonhangaba - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3538006
    cmbCidadeEmitente.AddItem "Piracicaba - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3538709
    cmbCidadeEmitente.AddItem "Pirajui - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3538907
    cmbCidadeEmitente.AddItem "Pirangi - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3539004
    cmbCidadeEmitente.AddItem "Pirapozinho - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3539202
    cmbCidadeEmitente.AddItem "Pirassununga - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3539301
    cmbCidadeEmitente.AddItem "Poa - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3539806
    cmbCidadeEmitente.AddItem "Pontal - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3540200
    cmbCidadeEmitente.AddItem "Porto Feliz - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3540606
    cmbCidadeEmitente.AddItem "Porto Ferreira - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3540705
    cmbCidadeEmitente.AddItem "Praia Grande - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3541000
    cmbCidadeEmitente.AddItem "Presidente Bernardes - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3541208
    cmbCidadeEmitente.AddItem "Presidente Prudente - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3541406
    cmbCidadeEmitente.AddItem "Presidente Venceslau - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3541505
    cmbCidadeEmitente.AddItem "Promissao - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3541604
    cmbCidadeEmitente.AddItem "Registro - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3542602
    cmbCidadeEmitente.AddItem "Ribeirao Pires - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3543303
    cmbCidadeEmitente.AddItem "Ribeirao Preto - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3543402
    cmbCidadeEmitente.AddItem "Rio Claro - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3543907
    cmbCidadeEmitente.AddItem "Rio das Pedras - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3544004
    cmbCidadeEmitente.AddItem "Saltinho - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3545159
    cmbCidadeEmitente.AddItem "Salto - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3545209
    cmbCidadeEmitente.AddItem "Salto de Pirapora - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3545308
    cmbCidadeEmitente.AddItem "Salto Grande - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3545407
    cmbCidadeEmitente.AddItem "Santa Barbara D Oeste - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3545803
    cmbCidadeEmitente.AddItem "Santa Cruz das Palmeiras - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3546306
    cmbCidadeEmitente.AddItem "Santa Gertrudes - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3546702
    cmbCidadeEmitente.AddItem "Santa Rosa de Viterbo - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3547601
    cmbCidadeEmitente.AddItem "Santo Anastacio - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3547700
    cmbCidadeEmitente.AddItem "Santo Andre - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3547809
    cmbCidadeEmitente.AddItem "Santo Antonio de Posse - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3548005
    cmbCidadeEmitente.AddItem "Santo Antonio do Pinhal - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3548203
    cmbCidadeEmitente.AddItem "Santos - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3548500
    cmbCidadeEmitente.AddItem "Sao Bernardo do Campo - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3548708
    cmbCidadeEmitente.AddItem "Sao Caetano do Sul - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3548807
    cmbCidadeEmitente.AddItem "Sao Carlos - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3548906
    cmbCidadeEmitente.AddItem "Sao Joao da Boa Vista - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3549102
    cmbCidadeEmitente.AddItem "Sao Joaquim da Barra - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3549409
    cmbCidadeEmitente.AddItem "Sao Jose do Rio Pardo - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3549706
    cmbCidadeEmitente.AddItem "Sao Jose do Rio Preto - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3549805
    cmbCidadeEmitente.AddItem "Sao Jose dos Campos - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3549904
    cmbCidadeEmitente.AddItem "Sao Paulo - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3550308
    cmbCidadeEmitente.AddItem "Sao Pedro - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3550407
    cmbCidadeEmitente.AddItem "Sao Roque - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3550605
    cmbCidadeEmitente.AddItem "Sao Sebastiao - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3550704
    cmbCidadeEmitente.AddItem "Sao Vicente - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3551009
    cmbCidadeEmitente.AddItem "Serra Azul - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3551405
    cmbCidadeEmitente.AddItem "Serrana - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3551504
    cmbCidadeEmitente.AddItem "Serra Negra - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3551603
    cmbCidadeEmitente.AddItem "Sertaozinho - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3551702
    cmbCidadeEmitente.AddItem "Socorro - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3552106
    cmbCidadeEmitente.AddItem "Sorocaba - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3552205
    cmbCidadeEmitente.AddItem "Sumare - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3552403
    cmbCidadeEmitente.AddItem "Suzano - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3552502
    cmbCidadeEmitente.AddItem "Tabapua - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3552601
    cmbCidadeEmitente.AddItem "Tabatinga - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3552700
    cmbCidadeEmitente.AddItem "Taboao da Serra - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3552809
    cmbCidadeEmitente.AddItem "Taquaritinga - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3553708
    cmbCidadeEmitente.AddItem "Tatui - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3554003
    cmbCidadeEmitente.AddItem "Taubate - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3554102
    cmbCidadeEmitente.AddItem "Teodoro Sampaio - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3554300
    cmbCidadeEmitente.AddItem "Tiete - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3554508
    cmbCidadeEmitente.AddItem "Tupa - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3555000
    cmbCidadeEmitente.AddItem "Tupi Paulista - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3555109
    cmbCidadeEmitente.AddItem "Ubatuba - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3555406
    cmbCidadeEmitente.AddItem "Valinhos - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3556206
    cmbCidadeEmitente.AddItem "Vargem - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3556354
    cmbCidadeEmitente.AddItem "Vargem Grande do Sul - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3556404
    cmbCidadeEmitente.AddItem "Vargem Grande Paulista - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3556453
    cmbCidadeEmitente.AddItem "Varzea Paulista - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3556503
    cmbCidadeEmitente.AddItem "Vinhedo - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3556701
    cmbCidadeEmitente.AddItem "Viradouro - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3556800
    cmbCidadeEmitente.AddItem "Vista Alegre do Alto - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3556909
    cmbCidadeEmitente.AddItem "Votorantim - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3557006
    cmbCidadeEmitente.AddItem "Votuporanga - SP"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 3557105
End Sub

Public Sub Parana(ByRef cmbCidadeEmitente As ComboBox)
    cmbCidadeEmitente.AddItem "Adrianopolis - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4100202
    cmbCidadeEmitente.AddItem "Agudos do Sul - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4100301
    cmbCidadeEmitente.AddItem "Almirante Tamandare - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4100400
    cmbCidadeEmitente.AddItem "Altonia - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4100509
    cmbCidadeEmitente.AddItem "Alto Parana - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4100608
    cmbCidadeEmitente.AddItem "Ampere - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4101002
    cmbCidadeEmitente.AddItem "Apucarana - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4101408
    cmbCidadeEmitente.AddItem "Arapongas - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4101507
    cmbCidadeEmitente.AddItem "Araucaria - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4101804
    cmbCidadeEmitente.AddItem "Assai - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4101903
    cmbCidadeEmitente.AddItem "Assis Chateaubriand - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4102000
    cmbCidadeEmitente.AddItem "Astorga - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4102109
    cmbCidadeEmitente.AddItem "Balsa Nova - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4102307
    cmbCidadeEmitente.AddItem "Bandeirantes - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4102406
    cmbCidadeEmitente.AddItem "Barracao - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4102604
    cmbCidadeEmitente.AddItem "Bela Vista da Caroba - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4102752
    cmbCidadeEmitente.AddItem "Bituruna - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4102901
    cmbCidadeEmitente.AddItem "Bocaiuva do Sul - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4103107
    cmbCidadeEmitente.AddItem "Bom Jesus do Sul - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4103156
    cmbCidadeEmitente.AddItem "Bom Sucesso do Sul - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4103222
    cmbCidadeEmitente.AddItem "Cafeara - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4103404
    cmbCidadeEmitente.AddItem "Cambe - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4103701
    cmbCidadeEmitente.AddItem "Campina Grande do Sul - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4103701
    cmbCidadeEmitente.AddItem "Campo Largo - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4104204
    cmbCidadeEmitente.AddItem "Campo Magro - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4104253
    cmbCidadeEmitente.AddItem "Campo Mourao - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4104303
    cmbCidadeEmitente.AddItem "Cantagalo - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4104451
    cmbCidadeEmitente.AddItem "Capanema - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4104501
    cmbCidadeEmitente.AddItem "Capitao Leonidas Marques - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4104600
    cmbCidadeEmitente.AddItem "Carambei - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4104659
    cmbCidadeEmitente.AddItem "Cascavel - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4104808
    cmbCidadeEmitente.AddItem "Castro - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4104907
    cmbCidadeEmitente.AddItem "Catanduvas - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4105003
    cmbCidadeEmitente.AddItem "Cerro Azul - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4105201
    cmbCidadeEmitente.AddItem "Ceu Azul - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4105300
    cmbCidadeEmitente.AddItem "Chopinzinho - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4105409
    cmbCidadeEmitente.AddItem "Cianorte - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4105508
    cmbCidadeEmitente.AddItem "Clevelandia - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4105706
    cmbCidadeEmitente.AddItem "Colombo - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4105805
    cmbCidadeEmitente.AddItem "Colorado - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4105904
    cmbCidadeEmitente.AddItem "Contenda - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4106209
    cmbCidadeEmitente.AddItem "Corbelia - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4106308
    cmbCidadeEmitente.AddItem "Cornelio Procopio - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4106407
    cmbCidadeEmitente.AddItem "Coronel Vivida - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4106506
    cmbCidadeEmitente.AddItem "Cruzeiro do Oeste - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4106605
    cmbCidadeEmitente.AddItem "Cruz Machado - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4106803
    cmbCidadeEmitente.AddItem "Curitiba - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4106902
    cmbCidadeEmitente.AddItem "Dois Vizinhos - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4107207
    cmbCidadeEmitente.AddItem "Douradina - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4107256
    cmbCidadeEmitente.AddItem "Engenheiro Beltrao - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4107504
    cmbCidadeEmitente.AddItem "Fazenda Rio Grande - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4107652
    cmbCidadeEmitente.AddItem "Fernandes Pinheiro - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4107736
    cmbCidadeEmitente.AddItem "Figueira - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4107751
    cmbCidadeEmitente.AddItem "Flor da Serra do Sul - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4107850
    cmbCidadeEmitente.AddItem "Formosa do Oeste - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4108205
    cmbCidadeEmitente.AddItem "Foz do Iguacu - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4108304
    cmbCidadeEmitente.AddItem "Francisco Alves - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4108320
    cmbCidadeEmitente.AddItem "Francisco Beltrao - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4108403
    cmbCidadeEmitente.AddItem "General Carneiro - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4108502
    cmbCidadeEmitente.AddItem "Goioere - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4108601
    cmbCidadeEmitente.AddItem "Guaira - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4108809
    cmbCidadeEmitente.AddItem "Guarapuava - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4109401
    cmbCidadeEmitente.AddItem "Guaraquecaba - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4109500
    cmbCidadeEmitente.AddItem "Guaratuba - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4109609
    cmbCidadeEmitente.AddItem "Ibipora - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4109807
    cmbCidadeEmitente.AddItem "Icaraima - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4109906
    cmbCidadeEmitente.AddItem "Inacio Martins - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4110201
    cmbCidadeEmitente.AddItem "Ipora - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4110607
    cmbCidadeEmitente.AddItem "Iracema do Oeste - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4110656
    cmbCidadeEmitente.AddItem "Irati - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4110706
    cmbCidadeEmitente.AddItem "Ivate - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4111555
    cmbCidadeEmitente.AddItem "Jaguariaiva - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4112009
    cmbCidadeEmitente.AddItem "Jandaia do Sul - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4112108
    cmbCidadeEmitente.AddItem "Jardim Alegre - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4112504
    cmbCidadeEmitente.AddItem "Juranda - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4112959
    cmbCidadeEmitente.AddItem "Lapa - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4113205
    cmbCidadeEmitente.AddItem "Laranjeiras do Sul - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4113304
    cmbCidadeEmitente.AddItem "Lindoeste - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4113452
    cmbCidadeEmitente.AddItem "Lobato - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4113601
    cmbCidadeEmitente.AddItem "Londrina - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4113700
    cmbCidadeEmitente.AddItem "Lupionopolis - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4113809
    cmbCidadeEmitente.AddItem "Mallet - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4113908
    cmbCidadeEmitente.AddItem "Mambore - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4114005
    cmbCidadeEmitente.AddItem "Mandaguacu - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4114104
    cmbCidadeEmitente.AddItem "Mandirituba - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4114302
    cmbCidadeEmitente.AddItem "Mangueirinha - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4114401
    cmbCidadeEmitente.AddItem "Marechal Candido Rondon - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4114609
    cmbCidadeEmitente.AddItem "Marialva - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4114807
    cmbCidadeEmitente.AddItem "Mariluz - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4115101
    cmbCidadeEmitente.AddItem "Maringa - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4115200
    cmbCidadeEmitente.AddItem "Mariopolis - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4115309
    cmbCidadeEmitente.AddItem "Marmeleiro - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4115408
    cmbCidadeEmitente.AddItem "Maua da Serra - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4115754
    cmbCidadeEmitente.AddItem "Medianeira - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4115804
    cmbCidadeEmitente.AddItem "Munhoz de Melo - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4116307
    cmbCidadeEmitente.AddItem "Nova Aurora - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4116703
    cmbCidadeEmitente.AddItem "Ortigueira - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4117305
    cmbCidadeEmitente.AddItem "Ourizona - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4117404
    cmbCidadeEmitente.AddItem "Paicandu - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4117503
    cmbCidadeEmitente.AddItem "Palmas - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4117602
    cmbCidadeEmitente.AddItem "Palmeira - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4117701
    cmbCidadeEmitente.AddItem "Palotina - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4117909
    cmbCidadeEmitente.AddItem "Paranagua - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4118204
    cmbCidadeEmitente.AddItem "Paranavai - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4118402
    cmbCidadeEmitente.AddItem "Pato Branco - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4118501
    cmbCidadeEmitente.AddItem "Paula Freitas - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4118600
    cmbCidadeEmitente.AddItem "Perola D Oeste - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4119004
    cmbCidadeEmitente.AddItem "Pinhais - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4119152
    cmbCidadeEmitente.AddItem "Pirai do Sul - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4119400
    cmbCidadeEmitente.AddItem "Ponta Grossa - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4119905
    cmbCidadeEmitente.AddItem "Porecatu - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4120002
    cmbCidadeEmitente.AddItem "Prado Ferreira - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4120333
    cmbCidadeEmitente.AddItem "Prudentopolis - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4120606
    cmbCidadeEmitente.AddItem "Quatro Barras - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4120804
    cmbCidadeEmitente.AddItem "Quatro Pontes - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4120853
    cmbCidadeEmitente.AddItem "Rancho Alegre - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4121307
    cmbCidadeEmitente.AddItem "Realeza - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4121406
    cmbCidadeEmitente.AddItem "Reboucas - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4121505
    cmbCidadeEmitente.AddItem "Renascenca - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4121604
    cmbCidadeEmitente.AddItem "Rio Azul - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4122008
    cmbCidadeEmitente.AddItem "Rio Branco do Sul - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4122206
    cmbCidadeEmitente.AddItem "Rio Negro - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4122305
    cmbCidadeEmitente.AddItem "Rolandia - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4122404
    cmbCidadeEmitente.AddItem "Rondon - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4122602
    cmbCidadeEmitente.AddItem "Salgado Filho - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4122800
    cmbCidadeEmitente.AddItem "Santa Helena - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4123501
    cmbCidadeEmitente.AddItem "Santa Izabel do Oeste - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4123808
    cmbCidadeEmitente.AddItem "Santa Tereza do Oeste - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4124020
    cmbCidadeEmitente.AddItem "Santo Antonio da Platina - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4124103
    cmbCidadeEmitente.AddItem "Santo Antonio do Sudoeste - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4124400
    cmbCidadeEmitente.AddItem "Sao Joao - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4124806
    cmbCidadeEmitente.AddItem "Sao Jorge D Oeste - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4125209
    cmbCidadeEmitente.AddItem "Sao Jorge do Ivai - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4125308
    cmbCidadeEmitente.AddItem "Sao Jose dos Pinhais - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4125506
    cmbCidadeEmitente.AddItem "Sao Mateus do Sul - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4125605
    cmbCidadeEmitente.AddItem "Sao Miguel do Iguacu - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4125704
    cmbCidadeEmitente.AddItem "Sao Pedro do Iguacu - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4125753
    cmbCidadeEmitente.AddItem "Sao Sebastiao da Amoreira - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4126009
    cmbCidadeEmitente.AddItem "Sarandi - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4126256
    cmbCidadeEmitente.AddItem "Sertanopolis - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4126504
    cmbCidadeEmitente.AddItem "Siqueira Campos - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4126603
    cmbCidadeEmitente.AddItem "Sulina - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4126652
    cmbCidadeEmitente.AddItem "Telemaco Borba - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4127106
    cmbCidadeEmitente.AddItem "Terra Boa - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4127205
    cmbCidadeEmitente.AddItem "Terra Roxa - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4127403
    cmbCidadeEmitente.AddItem "Tijucas do Sul - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4127601
    cmbCidadeEmitente.AddItem "Toledo - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4127700
    cmbCidadeEmitente.AddItem "Tomazina - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4127809
    cmbCidadeEmitente.AddItem "Umuarama - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4128104
    cmbCidadeEmitente.AddItem "Uniao da Vitoria - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4128203
    cmbCidadeEmitente.AddItem "Wenceslau Braz - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4128500
    cmbCidadeEmitente.AddItem "Vera Cruz do Oeste - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4128559
    cmbCidadeEmitente.AddItem "Vitorino - PR"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4128633
End Sub

Public Sub SantaCatarina(ByRef cmbCidadeEmitente As ComboBox)
    cmbCidadeEmitente.AddItem "Abdon Batista - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4200051
    cmbCidadeEmitente.AddItem "Agrolandia - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4200200
    cmbCidadeEmitente.AddItem "Agronomica - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4200309
    cmbCidadeEmitente.AddItem "Agua Doce - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4200408
    cmbCidadeEmitente.AddItem "Aguas de Chapeco - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4200507
    cmbCidadeEmitente.AddItem "Aguas Mornas - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4200606
    cmbCidadeEmitente.AddItem "Alfredo Wagner - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4200705
    cmbCidadeEmitente.AddItem "Alto Bela Vista - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4200754
    cmbCidadeEmitente.AddItem "Anchieta - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4200804
    cmbCidadeEmitente.AddItem "Anita Garibaldi - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4201000
    cmbCidadeEmitente.AddItem "Anitapolis - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4201109
    cmbCidadeEmitente.AddItem "Antonio Carlos - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4201208
    cmbCidadeEmitente.AddItem "Arabuta - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4201273
    cmbCidadeEmitente.AddItem "Araquari - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4201307
    cmbCidadeEmitente.AddItem "Ararangua - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4201406
    cmbCidadeEmitente.AddItem "Armazem - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4201505
    cmbCidadeEmitente.AddItem "ArroioTrinta - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4201604
    cmbCidadeEmitente.AddItem "Arvoredo - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4201653
    cmbCidadeEmitente.AddItem "Ascurra - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4201703
    cmbCidadeEmitente.AddItem "Atalanta - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4201802
    cmbCidadeEmitente.AddItem "Balneario Arroio do Silva - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4201950
    cmbCidadeEmitente.AddItem "Balneario Camboriu - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4202008
    cmbCidadeEmitente.AddItem "Balneario Gaivota - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4202073
    cmbCidadeEmitente.AddItem "Barra Velha - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4202107
    cmbCidadeEmitente.AddItem "Bela Vista do Toldo - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4202131
    cmbCidadeEmitente.AddItem "Belmonte - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4202156
    cmbCidadeEmitente.AddItem "Biguacu - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4202305
    cmbCidadeEmitente.AddItem "Blumenau - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4202404
    cmbCidadeEmitente.AddItem "Bombinhas - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4202453
    cmbCidadeEmitente.AddItem "Bom Jardim da Serra - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4202503
    cmbCidadeEmitente.AddItem "Bom Jesus - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4202537
    cmbCidadeEmitente.AddItem "Bom Jesus do Oeste - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4202578
    cmbCidadeEmitente.AddItem "Braco do Norte - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4202800
    cmbCidadeEmitente.AddItem "Brusque - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4202909
    cmbCidadeEmitente.AddItem "Cacador - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4203006
    cmbCidadeEmitente.AddItem "Caibi - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4203105
    cmbCidadeEmitente.AddItem "Camboriu - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4203204
    cmbCidadeEmitente.AddItem "Campo Alegre - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4203303
    cmbCidadeEmitente.AddItem "Campo Ere - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4203501
    cmbCidadeEmitente.AddItem "Campos Novos - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4203600
    cmbCidadeEmitente.AddItem "Canelinha - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4203709
    cmbCidadeEmitente.AddItem "Canoinhas - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4203808
    cmbCidadeEmitente.AddItem "Capinzal - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4203907
    cmbCidadeEmitente.AddItem "Catanduvas - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4204004
    cmbCidadeEmitente.AddItem "Celso Ramos - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4204152
    cmbCidadeEmitente.AddItem "Chapadao do Lageado - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4204194
    cmbCidadeEmitente.AddItem "Chapeco - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4204202
    cmbCidadeEmitente.AddItem "Cocal do Sul - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4204251
    cmbCidadeEmitente.AddItem "Concordia - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4204301
    cmbCidadeEmitente.AddItem "Cordilheira Alta - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4204350
    cmbCidadeEmitente.AddItem "Coronel Freitas - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4204400
    cmbCidadeEmitente.AddItem "Corupa - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4204509
    cmbCidadeEmitente.AddItem "Correia Pinto - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4204558
    cmbCidadeEmitente.AddItem "Criciuma - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4204608
    cmbCidadeEmitente.AddItem "Cunha Pora - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4204707
    cmbCidadeEmitente.AddItem "Cunhatai - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4204756
    cmbCidadeEmitente.AddItem "Curitibanos - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4204806
    cmbCidadeEmitente.AddItem "Descanso - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4204905
    cmbCidadeEmitente.AddItem "Dionisio Cerqueira - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4205001
    cmbCidadeEmitente.AddItem "Dona Emma - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4205100
    cmbCidadeEmitente.AddItem "Doutor Pedrinho - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4205159
    cmbCidadeEmitente.AddItem "Entre Rios - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4205175
    cmbCidadeEmitente.AddItem "Ermo - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4205191
    cmbCidadeEmitente.AddItem "Erval Velho - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4205209
    cmbCidadeEmitente.AddItem "Faxinal dos Guedes - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4205308
    cmbCidadeEmitente.AddItem "Flor do Sertao - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4205357
    cmbCidadeEmitente.AddItem "Florianopolis - SC"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4205407
    cmbCidadeEmitente.AddItem ("Forquilhinha - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4205456
    cmbCidadeEmitente.AddItem ("Fraiburgo - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4205506
    cmbCidadeEmitente.AddItem ("Frei Rogerio - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4205555
    cmbCidadeEmitente.AddItem ("Garopaba - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4205704
    cmbCidadeEmitente.AddItem ("Gaspar - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4205902
    cmbCidadeEmitente.AddItem ("Grao Para - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4206108
    cmbCidadeEmitente.AddItem ("Gravatal - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4206207
    cmbCidadeEmitente.AddItem ("Guaraciaba - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4206405
    cmbCidadeEmitente.AddItem ("Guaramirim - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4206504
    cmbCidadeEmitente.AddItem ("Guaruja do Sul - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4206603
    cmbCidadeEmitente.AddItem ("Guatambu - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4206652
    cmbCidadeEmitente.AddItem ("Herval D Oeste - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4206702
    cmbCidadeEmitente.AddItem ("Ibiam - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4206751
    cmbCidadeEmitente.AddItem ("Ibicare - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4206801
    cmbCidadeEmitente.AddItem ("Ibirama - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4206900
    cmbCidadeEmitente.AddItem ("Icara - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4207007
    cmbCidadeEmitente.AddItem ("Imarui - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4207205
    cmbCidadeEmitente.AddItem ("Imbituba - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4207304
    cmbCidadeEmitente.AddItem ("Imbuia - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4207403
    cmbCidadeEmitente.AddItem ("Indaial - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4207502
    cmbCidadeEmitente.AddItem ("Ipira - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4207601
    cmbCidadeEmitente.AddItem ("Ipora do Oeste - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4207650
    cmbCidadeEmitente.AddItem ("Ipuacu - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4207684
    cmbCidadeEmitente.AddItem ("Ipumirim - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4207700
    cmbCidadeEmitente.AddItem ("Irani - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4207809
    cmbCidadeEmitente.AddItem ("Irineopolis - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4207908
    cmbCidadeEmitente.AddItem ("Ita - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4208005
    cmbCidadeEmitente.AddItem ("Itajai - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4208203
    cmbCidadeEmitente.AddItem ("Itapema - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4208302
    cmbCidadeEmitente.AddItem ("Itapiranga - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4208401
    cmbCidadeEmitente.AddItem ("Ituporanga - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4208500
    cmbCidadeEmitente.AddItem ("Jacinto Machado - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4208708
    cmbCidadeEmitente.AddItem ("Jaguaruna - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4208807
    cmbCidadeEmitente.AddItem ("Jaragua do Sul - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4208906
    cmbCidadeEmitente.AddItem ("Jardinopolis - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4208955
    cmbCidadeEmitente.AddItem ("Joacaba - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4209003
    cmbCidadeEmitente.AddItem ("Joinville - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4209102
    cmbCidadeEmitente.AddItem ("Jupia - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4209177
    cmbCidadeEmitente.AddItem ("Lacerdopolis - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4209201
    cmbCidadeEmitente.AddItem ("Lages - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4209300
    cmbCidadeEmitente.AddItem ("Laguna - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4209409
    cmbCidadeEmitente.AddItem ("Lajeado Grande - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4209458
    cmbCidadeEmitente.AddItem ("Lauro Muller - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4209607
    cmbCidadeEmitente.AddItem ("Lebon Regis - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4209706
    cmbCidadeEmitente.AddItem ("Lindoia do Sul - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4209854
    cmbCidadeEmitente.AddItem ("Lontras - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4209904
    cmbCidadeEmitente.AddItem ("Luiz Alves - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4210001
    cmbCidadeEmitente.AddItem ("Luzerna - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4210035
    cmbCidadeEmitente.AddItem ("Macieira - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4210050
    cmbCidadeEmitente.AddItem ("Mafra - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4210100
    cmbCidadeEmitente.AddItem ("Major Gercino - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4210209
    cmbCidadeEmitente.AddItem ("Major Vieira - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4210308
    cmbCidadeEmitente.AddItem ("Maracaja - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4210407
    cmbCidadeEmitente.AddItem ("Maravilha - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4210506
    cmbCidadeEmitente.AddItem ("Marema - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4210555
    cmbCidadeEmitente.AddItem ("Massaranduba - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4210605
    cmbCidadeEmitente.AddItem ("Meleiro - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4210803
    cmbCidadeEmitente.AddItem ("Modelo - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4210902
    cmbCidadeEmitente.AddItem ("Mondai - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4211009
    cmbCidadeEmitente.AddItem ("Monte Carlo - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4211058
    cmbCidadeEmitente.AddItem ("Monte Castelo - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4211108
    cmbCidadeEmitente.AddItem ("Morro da Fumaca - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4211207
    cmbCidadeEmitente.AddItem ("Morro Grande - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4211256
    cmbCidadeEmitente.AddItem ("Navegantes - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4211306
    cmbCidadeEmitente.AddItem ("Nova Erechim - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4211405
    cmbCidadeEmitente.AddItem ("Nova Itaberaba - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4211454
    cmbCidadeEmitente.AddItem ("Nova Trento - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4211504
    cmbCidadeEmitente.AddItem ("Nova Veneza - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4211603
    cmbCidadeEmitente.AddItem ("Novo Horizonte - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4211652
    cmbCidadeEmitente.AddItem ("Orleans - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4211702
    cmbCidadeEmitente.AddItem ("Otacilio Costa - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4211751
    cmbCidadeEmitente.AddItem ("Ouro - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4211801
    cmbCidadeEmitente.AddItem ("Paial - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4211876
    cmbCidadeEmitente.AddItem ("Palhoca - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4211900
    cmbCidadeEmitente.AddItem ("Palma Sola - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4212007
    cmbCidadeEmitente.AddItem ("Palmitos - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4212106
    cmbCidadeEmitente.AddItem ("Papanduva - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4212205
    cmbCidadeEmitente.AddItem ("Passo de Torres - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4212254
    cmbCidadeEmitente.AddItem ("Passos Maia - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4212270
    cmbCidadeEmitente.AddItem ("Paulo Lopes - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4212304
    cmbCidadeEmitente.AddItem ("Pedras Grandes - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4212403
    cmbCidadeEmitente.AddItem ("Penha - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4212502
    cmbCidadeEmitente.AddItem ("Peritiba - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4212601
    cmbCidadeEmitente.AddItem ("Pescaria Brava - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4212650
    cmbCidadeEmitente.AddItem ("Petrolandia - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4212700
    cmbCidadeEmitente.AddItem ("Pinhalzinho - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4212908
    cmbCidadeEmitente.AddItem ("Piratuba - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4213104
    cmbCidadeEmitente.AddItem ("Planalto Alegre - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4213153
    cmbCidadeEmitente.AddItem ("Pomerode - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4213203
    cmbCidadeEmitente.AddItem ("Ponte Alta - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4213302
    cmbCidadeEmitente.AddItem ("Ponte Alta do Norte - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4213351
    cmbCidadeEmitente.AddItem ("Ponte Serrada - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4213401
    cmbCidadeEmitente.AddItem ("Porto Belo - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4213500
    cmbCidadeEmitente.AddItem ("Porto Uniao - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4213609
    cmbCidadeEmitente.AddItem ("Pouso Redondo - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4213708
    cmbCidadeEmitente.AddItem ("Praia Grande - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4213807
    cmbCidadeEmitente.AddItem ("Presidente Getulio - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4214003
    cmbCidadeEmitente.AddItem ("Presidente Nereu - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4214102
    cmbCidadeEmitente.AddItem ("Princesa - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 421415
    cmbCidadeEmitente.AddItem ("Quilombo - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4214201
    cmbCidadeEmitente.AddItem ("Rancho Queimado - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4214300
    cmbCidadeEmitente.AddItem ("Rio das Antas - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4214409
    cmbCidadeEmitente.AddItem ("Rio do Oeste - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4214607
    cmbCidadeEmitente.AddItem ("Rio dos Cedros - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4214706
    cmbCidadeEmitente.AddItem ("Rio do Sul - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4214805
    cmbCidadeEmitente.AddItem ("Rio Fortuna - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4214904
    cmbCidadeEmitente.AddItem ("Rio Negrinho - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4215000
    cmbCidadeEmitente.AddItem ("Rio Rufino - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4215059
    cmbCidadeEmitente.AddItem ("Riqueza - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4215075
    cmbCidadeEmitente.AddItem ("Saltinho - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4215356
    cmbCidadeEmitente.AddItem ("Salto Veloso - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4215406
    cmbCidadeEmitente.AddItem ("Sangao - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4215455
    cmbCidadeEmitente.AddItem ("Santa Cecilia - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4215505
    cmbCidadeEmitente.AddItem ("Santa Helena - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4215554
    cmbCidadeEmitente.AddItem ("Santa Rosa de Lima - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4215604
    cmbCidadeEmitente.AddItem ("Santa Rosa do Sul - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4215653
    cmbCidadeEmitente.AddItem ("Santa Terezinha - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4215679
    cmbCidadeEmitente.AddItem ("Santiago do Sul - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4215695
    cmbCidadeEmitente.AddItem ("Santo Amaro da Imperatriz - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4215703
    cmbCidadeEmitente.AddItem ("Sao Bento do Sul - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4215802
    cmbCidadeEmitente.AddItem ("Sao Bonifacio - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4215901
    cmbCidadeEmitente.AddItem ("Sao Carlos - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4216008
    cmbCidadeEmitente.AddItem ("Sao Cristovao do Sul - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4216057
    cmbCidadeEmitente.AddItem ("Sao Domingos - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4216107
    cmbCidadeEmitente.AddItem ("Sao Francisco do Sul - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4216206
    cmbCidadeEmitente.AddItem ("Sao Joao do Oeste - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4216255
    cmbCidadeEmitente.AddItem ("Sao Joao Batista - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4216305
    cmbCidadeEmitente.AddItem ("Sao Joao do Itaperiu - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4216354
    cmbCidadeEmitente.AddItem ("Sao Joao do Sul - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4216404
    cmbCidadeEmitente.AddItem ("Sao Joaquim - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4216503
    cmbCidadeEmitente.AddItem ("Sao Jose - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4216602
    cmbCidadeEmitente.AddItem ("Sao Jose do Cerrito - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4216800
    cmbCidadeEmitente.AddItem ("Sao Lourenco do Oeste - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4216909
    cmbCidadeEmitente.AddItem ("Sao Ludgero - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4217006
    cmbCidadeEmitente.AddItem ("Sao Martinho - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4217105
    cmbCidadeEmitente.AddItem ("Sao Miguel do Oeste - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4217204
    cmbCidadeEmitente.AddItem ("Saudades - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4217303
    cmbCidadeEmitente.AddItem ("Schroeder - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4217402
    cmbCidadeEmitente.AddItem ("Seara - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4217501
    cmbCidadeEmitente.AddItem ("Serra Alta - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4217550
    cmbCidadeEmitente.AddItem ("Sideropolis - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4217600
    cmbCidadeEmitente.AddItem ("Sombrio - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4217709
    cmbCidadeEmitente.AddItem ("Taio - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4217808
    cmbCidadeEmitente.AddItem ("Tangara - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4217907
    cmbCidadeEmitente.AddItem ("Tijucas - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4218004
    cmbCidadeEmitente.AddItem ("Timbo - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4218202
    cmbCidadeEmitente.AddItem ("Timbo Grande - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4218251
    cmbCidadeEmitente.AddItem ("Tres Barras - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4218301
    cmbCidadeEmitente.AddItem ("Treviso - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4218350
    cmbCidadeEmitente.AddItem ("Treze de Maio - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4218400
    cmbCidadeEmitente.AddItem ("Treze Tilias - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4218509
    cmbCidadeEmitente.AddItem ("Trombudo Central - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4218608
    cmbCidadeEmitente.AddItem ("Tubarao - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4218707
    cmbCidadeEmitente.AddItem ("Tunapolis - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4218756
    cmbCidadeEmitente.AddItem ("Turvo - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4218806
    cmbCidadeEmitente.AddItem ("Uniao do Oeste - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4218855
    cmbCidadeEmitente.AddItem ("Urussanga - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4219002
    cmbCidadeEmitente.AddItem ("Vargeao - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4219101
    cmbCidadeEmitente.AddItem ("Vargem Bonita - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4219176
    cmbCidadeEmitente.AddItem ("Vidal Ramos - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4219200
    cmbCidadeEmitente.AddItem ("Videira - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4219309
    cmbCidadeEmitente.AddItem ("Xanxere - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4219507
    cmbCidadeEmitente.AddItem ("Xavantina - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4219606
    cmbCidadeEmitente.AddItem ("Xaxim - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4219705
    cmbCidadeEmitente.AddItem ("Zortea - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4219853
    cmbCidadeEmitente.AddItem ("Balneario Rincao - SC")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4220000
End Sub

Public Sub RioGrandedoSul(ByRef cmbCidadeEmitente As ComboBox)
    cmbCidadeEmitente.AddItem "Agua Santa - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4300059
    cmbCidadeEmitente.AddItem "Alegrete - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4300406
    cmbCidadeEmitente.AddItem "Alpestre - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4300505
    cmbCidadeEmitente.AddItem "Alto Feliz - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4300570
    cmbCidadeEmitente.AddItem "Alvorada - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4300604
    cmbCidadeEmitente.AddItem "Amaral Ferrador - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4300638
    cmbCidadeEmitente.AddItem "Ametista do Sul - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4300646
    cmbCidadeEmitente.AddItem "Antonio Prado - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4300802
    cmbCidadeEmitente.AddItem "Arroio do Meio - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4301008
    cmbCidadeEmitente.AddItem "Arroio do Sal - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4301057
    cmbCidadeEmitente.AddItem "Arroio dos Ratos - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4301107
    cmbCidadeEmitente.AddItem "Bage - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4301602
    cmbCidadeEmitente.AddItem "Balneario Pinhal - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4301636
    cmbCidadeEmitente.AddItem "Barao de Cotergipe - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4301701
    cmbCidadeEmitente.AddItem "Barra Funda - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4301958
    cmbCidadeEmitente.AddItem "Bento Goncalves - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4302105
    cmbCidadeEmitente.AddItem "Boa vista do Burica - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4302204
    cmbCidadeEmitente.AddItem "Bom Principio - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4302352
    cmbCidadeEmitente.AddItem "Boqueirao do Leao - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4302451
    cmbCidadeEmitente.AddItem "Cacapava do Sul - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4302808
    cmbCidadeEmitente.AddItem "Cachoeira do Sul - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4303004
    cmbCidadeEmitente.AddItem "Cachoeirinha - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4303103
    cmbCidadeEmitente.AddItem "Caibate - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4303301
    cmbCidadeEmitente.AddItem "Caicara - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4303400
    cmbCidadeEmitente.AddItem "Camaqua - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4303509
    cmbCidadeEmitente.AddItem "Campestre da Serra - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4303673
    cmbCidadeEmitente.AddItem "Campinas do Sul - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4303806
    cmbCidadeEmitente.AddItem "Campo Bom - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4303905
    cmbCidadeEmitente.AddItem "Campo Novo - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4304002
    cmbCidadeEmitente.AddItem "Candiota - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4304358
    cmbCidadeEmitente.AddItem "Canela - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4304408
    cmbCidadeEmitente.AddItem "Cangucu - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4304507
    cmbCidadeEmitente.AddItem "Canoas - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4304606
    cmbCidadeEmitente.AddItem "Capao da Canoa - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4304630
    cmbCidadeEmitente.AddItem "Carazinho - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4304705
    cmbCidadeEmitente.AddItem "Carlos Barbosa - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4304804
    cmbCidadeEmitente.AddItem "Casca - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4304903
    cmbCidadeEmitente.AddItem "Caxias do Sul - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4305108
    cmbCidadeEmitente.AddItem "Cerro Grande do Sul - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4305173
    cmbCidadeEmitente.AddItem "Cerro Largo - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4305207
    cmbCidadeEmitente.AddItem "Chapada - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4305306
    cmbCidadeEmitente.AddItem "Charqueadas - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4305355
    cmbCidadeEmitente.AddItem "Condor - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4305702
    cmbCidadeEmitente.AddItem "Constantina - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4305801
    cmbCidadeEmitente.AddItem "Coqueiros do Sul - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4305850
    cmbCidadeEmitente.AddItem "Crissiumal - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4306007
    cmbCidadeEmitente.AddItem "Cristal do Sul - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4306072
    cmbCidadeEmitente.AddItem "Cruz Alta - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4306106
    cmbCidadeEmitente.AddItem "David Canabarro - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4306304
    cmbCidadeEmitente.AddItem "Dois Irmaos - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4306403
    cmbCidadeEmitente.AddItem "Dom Feliciano - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4306502
    cmbCidadeEmitente.AddItem "Dom Pedrito - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4306601
    cmbCidadeEmitente.AddItem "Doutor Mauricio Cardoso - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4306734
    cmbCidadeEmitente.AddItem "Eldorado do Sul - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4306767
    cmbCidadeEmitente.AddItem "Encantado - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4306809
    cmbCidadeEmitente.AddItem "Erechim - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4307005
    cmbCidadeEmitente.AddItem "Ernestina - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4307054
    cmbCidadeEmitente.AddItem "Espumoso - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4307500
    cmbCidadeEmitente.AddItem "Estacao - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4307559
    cmbCidadeEmitente.AddItem "Estancia Velha - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4307609
    cmbCidadeEmitente.AddItem "Esteio - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4307708
    cmbCidadeEmitente.AddItem "Estrela - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4307807
    cmbCidadeEmitente.AddItem "Farroupilha - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4307906
    cmbCidadeEmitente.AddItem "Feliz - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4308102
    cmbCidadeEmitente.AddItem "Flores da Cunha - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4308201
    cmbCidadeEmitente.AddItem "Fortaleza dos Valos - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4308458
    cmbCidadeEmitente.AddItem "Frederico Westphalen - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4308508
    cmbCidadeEmitente.AddItem "Garibaldi - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4308607
    cmbCidadeEmitente.AddItem "Gaurama - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4308706
    cmbCidadeEmitente.AddItem "Getulio Vargas - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4308904
    cmbCidadeEmitente.AddItem "Girua - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4309001
    cmbCidadeEmitente.AddItem "Glorinha - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4309050
    cmbCidadeEmitente.AddItem "Gramado - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4309100
    cmbCidadeEmitente.AddItem "Gravatai - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4309209
    cmbCidadeEmitente.AddItem "Guaiba - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4309308
    cmbCidadeEmitente.AddItem "Guapore - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4309407
    cmbCidadeEmitente.AddItem "Guarani das Missoes - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4309506
    cmbCidadeEmitente.AddItem "Horizontina - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4309605
    cmbCidadeEmitente.AddItem "Ibiaca - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4309803
    cmbCidadeEmitente.AddItem "Ibiraiaras - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4309902
    cmbCidadeEmitente.AddItem "Ibiruba - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4310009
    cmbCidadeEmitente.AddItem "Ijui - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4310207
    cmbCidadeEmitente.AddItem "Imbe - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4310330
    cmbCidadeEmitente.AddItem "Independencia - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4310405
    cmbCidadeEmitente.AddItem "Ipe - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4310439
    cmbCidadeEmitente.AddItem "Irai - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4310504
    cmbCidadeEmitente.AddItem "Itaqui - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4310603
    cmbCidadeEmitente.AddItem "Ivoti - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4310801
    cmbCidadeEmitente.AddItem "Jaboticaba - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4310850
    cmbCidadeEmitente.AddItem "Jacutinga - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4310900
    cmbCidadeEmitente.AddItem "Jaguarao - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4311007
    cmbCidadeEmitente.AddItem "Julio de Castilhos - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4311205
    cmbCidadeEmitente.AddItem "Lagoa Bonita do Sul - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4311239
    cmbCidadeEmitente.AddItem "Lagoa dos Tres Cantos - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4311270
    cmbCidadeEmitente.AddItem "Lagoa Vermelha - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4311304
    cmbCidadeEmitente.AddItem "Lajeado - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4311403
    cmbCidadeEmitente.AddItem "Marau - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4311809
    cmbCidadeEmitente.AddItem "Monte Belo do Sul - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4312385
    cmbCidadeEmitente.AddItem "Montenegro - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4312401
    cmbCidadeEmitente.AddItem "Morro Redondo - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4312450
    cmbCidadeEmitente.AddItem "Morro Reuter - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4312476
    cmbCidadeEmitente.AddItem "Mucum - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4312609
    cmbCidadeEmitente.AddItem "Nao-Me-Toque - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4312658
    cmbCidadeEmitente.AddItem "Nicolau Vergueiro - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4312674
    cmbCidadeEmitente.AddItem "Nonoai - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4312708
    cmbCidadeEmitente.AddItem "Nova Alvorada - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4312757
    cmbCidadeEmitente.AddItem "Nova Araca - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4312807
    cmbCidadeEmitente.AddItem "Nova Bassano - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4312906
    cmbCidadeEmitente.AddItem "Nova Boa Vista - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4312955
    cmbCidadeEmitente.AddItem "Nova Esperanca do Sul - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4313037
    cmbCidadeEmitente.AddItem "Nova Petropolis - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4313201
    cmbCidadeEmitente.AddItem "Nova Prata - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4313300
    cmbCidadeEmitente.AddItem "Nova Hamburgo - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4313409
    cmbCidadeEmitente.AddItem "Nova Barreiro - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4313490
    cmbCidadeEmitente.AddItem "Osorio - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4313508
    cmbCidadeEmitente.AddItem "Palmeira das Missoes - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4313706
    cmbCidadeEmitente.AddItem "Palmitinho - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4313805
    cmbCidadeEmitente.AddItem "Panambi - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4313904
    cmbCidadeEmitente.AddItem "Pantano Grande - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4313953
    cmbCidadeEmitente.AddItem "Parai - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4314001
    cmbCidadeEmitente.AddItem "Parobe - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4314050
    cmbCidadeEmitente.AddItem "Passo Fundo - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4314100
    cmbCidadeEmitente.AddItem "Pelotas - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4314407
    cmbCidadeEmitente.AddItem "Picada Cafe - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4314423
    cmbCidadeEmitente.AddItem "Pinhal - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4314456
    cmbCidadeEmitente.AddItem "Planalto - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4314704
    cmbCidadeEmitente.AddItem "Pontao - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4314779
    cmbCidadeEmitente.AddItem "Portao - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4314803
    cmbCidadeEmitente.AddItem "Porto Alegre - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4314902
    cmbCidadeEmitente.AddItem "Porto Maua - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4315057
    cmbCidadeEmitente.AddItem "Porto Xavier - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4315107
    cmbCidadeEmitente.AddItem "Redentora - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4315404
    cmbCidadeEmitente.AddItem "Rio Grande - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4315602
    cmbCidadeEmitente.AddItem "Rodeio Bonito - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4315909
    cmbCidadeEmitente.AddItem "Rolante - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4316006
    cmbCidadeEmitente.AddItem "Ronda Alta - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4316105
    cmbCidadeEmitente.AddItem "Rondinha - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4316204
    cmbCidadeEmitente.AddItem "Sananduva - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4316600
    cmbCidadeEmitente.AddItem "Santa Clara do Sul - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4316758
    cmbCidadeEmitente.AddItem "Santa Cruz do Sul - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4316808
    cmbCidadeEmitente.AddItem "Santa Maria - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4316907
    cmbCidadeEmitente.AddItem "Santana do Livramento - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4317103
    cmbCidadeEmitente.AddItem "Santa Rosa - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4317202
    cmbCidadeEmitente.AddItem "Santiago - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4317400
    cmbCidadeEmitente.AddItem "Santo Angelo - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4317509
    cmbCidadeEmitente.AddItem "Santo Antonio da Patrulha - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4317608
    cmbCidadeEmitente.AddItem "Santo Antonio das Missoes - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4317707
    cmbCidadeEmitente.AddItem "Santo Antonio do Planalto - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4317756
    cmbCidadeEmitente.AddItem "Santo Augusto - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4317806
    cmbCidadeEmitente.AddItem "Santo Cristo - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4317905
    cmbCidadeEmitente.AddItem "Santo Borja - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4318002
    cmbCidadeEmitente.AddItem "Sao Domingos do Sul - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4318051
    cmbCidadeEmitente.AddItem "Sao Francisco de Paula - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4318200
    cmbCidadeEmitente.AddItem "Sao Gabriel - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4318309
    cmbCidadeEmitente.AddItem "Sao Jeronimo - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4318408
    cmbCidadeEmitente.AddItem "Sao Joao da Urtiga - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4318424
    cmbCidadeEmitente.AddItem "Sao Jose das Missoes - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4318457
    cmbCidadeEmitente.AddItem "Sao Jose do Ouro - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4318606
    cmbCidadeEmitente.AddItem "Sao Leopoldo - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4318705
    cmbCidadeEmitente.AddItem "Sao Luiz Gonzaga - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4318903
    cmbCidadeEmitente.AddItem "Sao Miguel das Missoes - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4319158
    cmbCidadeEmitente.AddItem "Sao Pedro do Butia - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4319372
    cmbCidadeEmitente.AddItem "Sao Sebastiao do Cai - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4319505
    cmbCidadeEmitente.AddItem "Sao Sepe - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4319604
    cmbCidadeEmitente.AddItem "Sapiranga - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4319901
    cmbCidadeEmitente.AddItem "Sapucaia do Sul - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4320008
    cmbCidadeEmitente.AddItem "Sarandi - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4320107
    cmbCidadeEmitente.AddItem "Seberi - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4320206
    cmbCidadeEmitente.AddItem "Serafina Correa - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4320404
    cmbCidadeEmitente.AddItem "Sertao Santana - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4320552
    cmbCidadeEmitente.AddItem "Soledade - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4320800
    cmbCidadeEmitente.AddItem "Tapejara - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4320909
    cmbCidadeEmitente.AddItem "Tapera - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4321006
    cmbCidadeEmitente.AddItem "Taquara - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4321204
    cmbCidadeEmitente.AddItem "Taquari - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4321303
    cmbCidadeEmitente.AddItem "Tenente Portela - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4321402
    cmbCidadeEmitente.AddItem "Terra de Areia - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4321436
    cmbCidadeEmitente.AddItem "Teutonia - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4321451
    cmbCidadeEmitente.AddItem "Tio Hugo - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4321469
    cmbCidadeEmitente.AddItem "Torres - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4321501
    cmbCidadeEmitente.AddItem "Tramandai - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4321600
    cmbCidadeEmitente.AddItem "Tres Arroios - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4321634
    cmbCidadeEmitente.AddItem "Tres Cachoeiras - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4321667
    cmbCidadeEmitente.AddItem "Tres Coroas - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4321709
    cmbCidadeEmitente.AddItem "Tres de Maio - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4321808
    cmbCidadeEmitente.AddItem "Tres Passos - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4321907
    cmbCidadeEmitente.AddItem "Trindade do Sul - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4321956
    cmbCidadeEmitente.AddItem "Triunfo - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4322004
    cmbCidadeEmitente.AddItem "Tucunduva - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4322103
    cmbCidadeEmitente.AddItem "Tuparendi - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4322301
    cmbCidadeEmitente.AddItem "Uruguaiana - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4322400
    cmbCidadeEmitente.AddItem "Vacaria - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4322509
    cmbCidadeEmitente.AddItem "Vanini - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4322558
    cmbCidadeEmitente.AddItem "Venancio Aires - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4322608
    cmbCidadeEmitente.AddItem "Vera Cruz - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4322707
    cmbCidadeEmitente.AddItem "Veranopolis - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4322806
    cmbCidadeEmitente.AddItem "Viamao - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4323002
    cmbCidadeEmitente.AddItem "Vila Langaro - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4323358
    cmbCidadeEmitente.AddItem "Vila Alegre - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4323507
    cmbCidadeEmitente.AddItem "Westfalia - RS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 4323770
End Sub

Public Sub MatoGrossodoSul(ByRef cmbCidadeEmitente As ComboBox)
    cmbCidadeEmitente.AddItem "Anastacio - MS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5000708
    cmbCidadeEmitente.AddItem "Aquidauana - MS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5001102
    cmbCidadeEmitente.AddItem "Aral Moreira - MS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5001243
    cmbCidadeEmitente.AddItem "Bataguassu - MS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5001904
    cmbCidadeEmitente.AddItem "Bataypora - MS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5002001
    cmbCidadeEmitente.AddItem "Bela Vista - MS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5002100
    cmbCidadeEmitente.AddItem "Bodoquena - MS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5002159
    cmbCidadeEmitente.AddItem "Bonito - MS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5002209
    cmbCidadeEmitente.AddItem "Brasilandia - MS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5002308
    cmbCidadeEmitente.AddItem "Caarapo - MS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5002407
    cmbCidadeEmitente.AddItem "Campo Grande - MS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5002704
    cmbCidadeEmitente.AddItem "Coronel Sapucaia - MS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5003157
    cmbCidadeEmitente.AddItem "Corumba - MS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5003207
    cmbCidadeEmitente.AddItem "Deodapolis - MS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5003454
    cmbCidadeEmitente.AddItem "Dourados - MS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5003702
    cmbCidadeEmitente.AddItem "Fatima do Sul - MS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5003801
    cmbCidadeEmitente.AddItem "Guia Lopes da Laguna - MS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5004106
    cmbCidadeEmitente.AddItem "Iguatemi - MS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5004304
    cmbCidadeEmitente.AddItem "Inocencia - MS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5004403
    cmbCidadeEmitente.AddItem "Itaquirai - MS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5004601
    cmbCidadeEmitente.AddItem "Ivinhema - MS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5004700
    cmbCidadeEmitente.AddItem "Jardim - MS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5005004
    cmbCidadeEmitente.AddItem "Maracaju - MS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5005400
    cmbCidadeEmitente.AddItem "Miranda - MS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5005608
    cmbCidadeEmitente.AddItem "Mundo Novo - MS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5005681
    cmbCidadeEmitente.AddItem "Navirai - MS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5005707
    cmbCidadeEmitente.AddItem "Nova Alvorada do Sul - MS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5006002
    cmbCidadeEmitente.AddItem "Nova Andradina - MS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5006200
    cmbCidadeEmitente.AddItem "Novo Horizonte do Sul - MS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5006259
    cmbCidadeEmitente.AddItem "Paranaiba - MS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5006309
    cmbCidadeEmitente.AddItem "Ponta Pora - MS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5006606
    cmbCidadeEmitente.AddItem "Porto Murtinho - MS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5006903
    cmbCidadeEmitente.AddItem "Ribas do Rio Pardo - MS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5007109
    cmbCidadeEmitente.AddItem "Rio Brilhante - MS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5007208
    cmbCidadeEmitente.AddItem "Rio Verde de Mato Grosso - MS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5007406
    cmbCidadeEmitente.AddItem "Santa Rita do Pardo - MS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5007554
    cmbCidadeEmitente.AddItem "Sao Gabriel do Oeste - MS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5007695
    cmbCidadeEmitente.AddItem "Selviria - MS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5007802
    cmbCidadeEmitente.AddItem "Sidrolandia - MS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5007901
    cmbCidadeEmitente.AddItem "Taquarussu - MS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5007976
    cmbCidadeEmitente.AddItem "Tres Lagoas - MS"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5008305
End Sub

Public Sub MatoGrosso(ByRef cmbCidadeEmitente As ComboBox)
    cmbCidadeEmitente.AddItem "Agua Boa - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5100201
    cmbCidadeEmitente.AddItem "Alta Floresta - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5100250
    cmbCidadeEmitente.AddItem "Alto Araguaia - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5100300
    cmbCidadeEmitente.AddItem "Alto Garcas - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5100409
    cmbCidadeEmitente.AddItem "Apiacas - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5100805
    cmbCidadeEmitente.AddItem "Arenapolis - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5101308
    cmbCidadeEmitente.AddItem "Barra do Garcas - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5101803
    cmbCidadeEmitente.AddItem "Brasnorte - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5101902
    cmbCidadeEmitente.AddItem "Caceres - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5102504
    cmbCidadeEmitente.AddItem "Campo Novo do Parecis - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5102637
    cmbCidadeEmitente.AddItem "Campo Verde - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5102678
    cmbCidadeEmitente.AddItem "Campo de Julio - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5102686
    cmbCidadeEmitente.AddItem "Canarana - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5102702
    cmbCidadeEmitente.AddItem "Chapada dos Guimaraes - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5103007
    cmbCidadeEmitente.AddItem "Colider - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5103205
    cmbCidadeEmitente.AddItem "Colniza - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5103254
    cmbCidadeEmitente.AddItem "Comodoro - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5103304
    cmbCidadeEmitente.AddItem "Confresa - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5103353
    cmbCidadeEmitente.AddItem "Conquista do Oeste - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5103361
    cmbCidadeEmitente.AddItem "Cotriguacu - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5103379
    cmbCidadeEmitente.AddItem "Cuiaba - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5103403
    cmbCidadeEmitente.AddItem "Denise - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5103452
    cmbCidadeEmitente.AddItem "Diamantino - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5103502
    cmbCidadeEmitente.AddItem "Feliz Natal - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5103700
    cmbCidadeEmitente.AddItem "Guaranta do Norte - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5104104
    cmbCidadeEmitente.AddItem "Ipiranga do Norte - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5104526
    cmbCidadeEmitente.AddItem "Itanhanga - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5104542
    cmbCidadeEmitente.AddItem "Itauba - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5104559
    cmbCidadeEmitente.AddItem "Itiquira - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5104609
    cmbCidadeEmitente.AddItem "Jaciara - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5104807
    cmbCidadeEmitente.AddItem "Juara - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5105101
    cmbCidadeEmitente.AddItem "Juina - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5105150
    cmbCidadeEmitente.AddItem "Lucas do Rio Verde - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5105259
    cmbCidadeEmitente.AddItem "Matupa - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5105606
    cmbCidadeEmitente.AddItem "Mirassol D Oeste - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5105622
    cmbCidadeEmitente.AddItem "Nobres - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5105903
    cmbCidadeEmitente.AddItem "Nova Brasilandia - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5106208
    cmbCidadeEmitente.AddItem "Nova Mutum - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5106224
    cmbCidadeEmitente.AddItem "Nova Olimpia - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5106232
    cmbCidadeEmitente.AddItem "Nova Xavantina - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5106257
    cmbCidadeEmitente.AddItem "Paranatinga - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5106307
    cmbCidadeEmitente.AddItem "Pedra Preta - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5106372
    cmbCidadeEmitente.AddItem "Peixoto de Azevedo - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5106422
    cmbCidadeEmitente.AddItem "Planalto da Serra - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5106455
    cmbCidadeEmitente.AddItem "Pocone - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5106505
    cmbCidadeEmitente.AddItem "Pontes E Lacerda - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5106752
    cmbCidadeEmitente.AddItem "Porto Alegre do Norte - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5106778
    cmbCidadeEmitente.AddItem "Poxoreo - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5107008
    cmbCidadeEmitente.AddItem "Primavera do Leste - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5107040
    cmbCidadeEmitente.AddItem "Querencia - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5107065
    cmbCidadeEmitente.AddItem "Sao Jose Dos Quatro Marcos - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5107107
    cmbCidadeEmitente.AddItem "Ribeirao Cascalheira - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5107180
    cmbCidadeEmitente.AddItem "Santa Carmen - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5107248
    cmbCidadeEmitente.AddItem "Sao Jose do Rio Claro - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5107305
    cmbCidadeEmitente.AddItem "Rondonopolis - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5107602
    cmbCidadeEmitente.AddItem "Santa Rita do Trivelato - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5107768
    cmbCidadeEmitente.AddItem "Santo Antonio do Leverger - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5107800
    cmbCidadeEmitente.AddItem "Sao Felix do Araguaia - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5107859
    cmbCidadeEmitente.AddItem "Sapezal - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5107875
    cmbCidadeEmitente.AddItem "Sinop - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5107909
    cmbCidadeEmitente.AddItem "Sorriso - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5107925
    cmbCidadeEmitente.AddItem "Tangara da Serra - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5107958
    cmbCidadeEmitente.AddItem "Tapurah - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5108006
    cmbCidadeEmitente.AddItem "Torixoreu - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5108204
    cmbCidadeEmitente.AddItem "Vale de Sao Domingos - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5108352
    cmbCidadeEmitente.AddItem "Varzea Grande - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5108402
    cmbCidadeEmitente.AddItem "Vila Rica - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5108600
    cmbCidadeEmitente.AddItem "Nova Maringa - MT"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5108907
End Sub

Public Sub Goias(ByRef cmbCidadeEmitente As ComboBox)
    cmbCidadeEmitente.AddItem "Acreuna - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5200134
    cmbCidadeEmitente.AddItem "Adelandia - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5200159
    cmbCidadeEmitente.AddItem "Agua Limpa - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5200209
    cmbCidadeEmitente.AddItem "Aguas Lindas de Goias - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5200258
    cmbCidadeEmitente.AddItem "Anapolis - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5201108
    cmbCidadeEmitente.AddItem "Aparecida de Goiania - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5201405
    cmbCidadeEmitente.AddItem "Aparecida do Rio Doce - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5201454
    cmbCidadeEmitente.AddItem "Apore - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5201504
    cmbCidadeEmitente.AddItem "Aracu - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5201603
    cmbCidadeEmitente.AddItem "Aragarcas - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5201702
    cmbCidadeEmitente.AddItem "Aragoiania - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5201801
    cmbCidadeEmitente.AddItem "Brazabrantes - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5203609
    cmbCidadeEmitente.AddItem "Cabeceiras - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5204003
    cmbCidadeEmitente.AddItem "Cacu - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5204300
    cmbCidadeEmitente.AddItem "Caiaponia - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5204409
    cmbCidadeEmitente.AddItem "Caldas Novas - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5204508
    cmbCidadeEmitente.AddItem "Campestre de Goias - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5204607
    cmbCidadeEmitente.AddItem "Carmo do Rio Verde - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5205000
    cmbCidadeEmitente.AddItem "Catalao - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5205109
    cmbCidadeEmitente.AddItem "Ceres - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5205406
    cmbCidadeEmitente.AddItem "Cezarina - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5205455
    cmbCidadeEmitente.AddItem "Chapadao do Ceu - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5205471
    cmbCidadeEmitente.AddItem "Corumbá de Goiás - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5205802
    cmbCidadeEmitente.AddItem "Crixas - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5206404
    cmbCidadeEmitente.AddItem "Edeia - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5207402
    cmbCidadeEmitente.AddItem "Faina - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5207535
    cmbCidadeEmitente.AddItem "Flores de Goias - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5207907
    cmbCidadeEmitente.AddItem "Formosa - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5208004
    cmbCidadeEmitente.AddItem "Goianesia - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5208608
    cmbCidadeEmitente.AddItem "Goiania - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5208707
    cmbCidadeEmitente.AddItem "Goianira - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5208806
    cmbCidadeEmitente.AddItem "Goias - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5208905
    cmbCidadeEmitente.AddItem "Guaraita - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5209291
    cmbCidadeEmitente.AddItem "Heitorai - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5209606
    cmbCidadeEmitente.AddItem "Indiara - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5209952
    cmbCidadeEmitente.AddItem "Inhumas - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5210000
    cmbCidadeEmitente.AddItem "Ipora - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5210208
    cmbCidadeEmitente.AddItem "Itaberai - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5210406
    cmbCidadeEmitente.AddItem "Itaja - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5210802
    cmbCidadeEmitente.AddItem "Itapirapua - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5211008
    cmbCidadeEmitente.AddItem "Itaruma - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5211305
    cmbCidadeEmitente.AddItem "Itumbiara - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5211503
    cmbCidadeEmitente.AddItem "Jandaia - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5211701
    cmbCidadeEmitente.AddItem "Jatai - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5211909
    cmbCidadeEmitente.AddItem "Lagoa Santa - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5212253
    cmbCidadeEmitente.AddItem "Luziania - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5212501
    cmbCidadeEmitente.AddItem "Mambai - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5212709
    cmbCidadeEmitente.AddItem "Marzagao - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5212907
    cmbCidadeEmitente.AddItem "Mineiros - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5213103
    cmbCidadeEmitente.AddItem "Monte Alegre de Goias - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5213509
    cmbCidadeEmitente.AddItem "Montividiu - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5213756
    cmbCidadeEmitente.AddItem "Mozarlandia - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5214002
    cmbCidadeEmitente.AddItem "Mundo Novo - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5214051
    cmbCidadeEmitente.AddItem "Neropolis - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5214507
    cmbCidadeEmitente.AddItem "Nova Crixas - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5214838
    cmbCidadeEmitente.AddItem "Nova Veneza - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5215009
    cmbCidadeEmitente.AddItem "Orizona - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5215306
    cmbCidadeEmitente.AddItem "Ouro Verde de Goias - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5215405
    cmbCidadeEmitente.AddItem "Paranaiguara - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5216304
    cmbCidadeEmitente.AddItem "Parauna - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5216403
    cmbCidadeEmitente.AddItem "Piracanjuba - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5217104
    cmbCidadeEmitente.AddItem "Pirenopolis - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5217302
    cmbCidadeEmitente.AddItem "Pires do Rio - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5217401
    cmbCidadeEmitente.AddItem "Planaltina - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5217609
    cmbCidadeEmitente.AddItem "Pontalina - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5217708
    cmbCidadeEmitente.AddItem "Porangatu - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5218003
    cmbCidadeEmitente.AddItem "Posse - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5218300
    cmbCidadeEmitente.AddItem "Rio Verde - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5218805
    cmbCidadeEmitente.AddItem "Rubiataba - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5218904
    cmbCidadeEmitente.AddItem "Santa Fe de Goias - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5219258
    cmbCidadeEmitente.AddItem "Sao Luis de Montes Belos - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5220108
    cmbCidadeEmitente.AddItem "Sao Miguel do Araguaia - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5220207
    cmbCidadeEmitente.AddItem "Senador Canedo - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5220454
    cmbCidadeEmitente.AddItem "Silvania - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5220603
    cmbCidadeEmitente.AddItem "Terezopolis de Goias - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5221197
    cmbCidadeEmitente.AddItem "Trindade - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5221403
    cmbCidadeEmitente.AddItem "Uruacu - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5221601
    cmbCidadeEmitente.AddItem "Valparaiso de Goias - GO"
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5221858
End Sub

Public Sub Brasilia(ByRef cmbCidadeEmitente As ComboBox)
    cmbCidadeEmitente.AddItem ("Brasilia - DF")
    cmbCidadeEmitente.ItemData(cmbCidadeEmitente.NewIndex) = 5300108
End Sub

Private Sub cmbCidadeEmitente_Click()
    Dim cidadeSelecionada As String
    Dim CodigoMunicipio As String
    
    cidadeSelecionada = cmbCidadeEmitente.Text
    CodigoMunicipio = cmbCidadeEmitente.ItemData(cmbCidadeEmitente.ListIndex)
End Sub
