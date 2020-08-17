unit frmCalculoFrenet;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, Mask, ACBrBase, ACBrSocket, ACBrSedex,
  TypInfo, ExtCtrls, DB, DBClient, Grids, DBGrids, ComCtrls, TabNotBk,
  DBCtrls, MidasLib, ACBrFrenet, XPMan;

type
  TForm1 = class(TForm)
    XPManifest1: TXPManifest;
    Frenet: TACBrFrenet;
    Panel1: TPanel;
    Panel4: TPanel;
    btnConsultar: TButton;
    Panel3: TPanel;
    Label22: TLabel;
    mRetFrenet: TMemo;
    cdItens: TClientDataSet;
    dsItens: TDataSource;
    DBGrid2: TDBGrid;
    cdCotacao: TClientDataSet;
    dsCotacao: TDataSource;
    Panel2: TPanel;
    Panel5: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label11: TLabel;
    Image1: TImage;
    EditCEPOrigem: TEdit;
    EditCEPDestino: TEdit;
    EditValorDeclarado: TEdit;
    Panel6: TPanel;
    DBGrid1: TDBGrid;
    Label3: TLabel;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    Label4: TLabel;
    EditToken: TEdit;
    cdItensPeso: TFloatField;
    cdItensComprimento: TFloatField;
    cdItensAltura: TFloatField;
    cdItensLargura: TFloatField;
    cdItensSKU: TStringField;
    cdItensCategoria: TStringField;
    cdItensQuantidade: TIntegerField;
    cdItensDiametro: TFloatField;
    //btnServicos: TButton;
    //cdItensDiametro: TFloatField;
    btnServicos: TButton;
    procedure btnConsultarClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure Label4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnServicosClick(Sender: TObject);
  private

    procedure LerRetorno;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}


uses
  ACBrUtil;


procedure TForm1.btnConsultarClick(Sender: TObject);
begin

  try
    Frenet.Token := EditToken.Text;
    Frenet.CepOrigem := EditCEPOrigem.Text;
    Frenet.CepDestino := EditCEPDestino.Text;
    Frenet.ValorDeclarado := StringToFloatDef(EditValorDeclarado.Text, 0);

    // Apagar itens anteriores
    Frenet.Itens.Clear;

    if (cdItens.State in [dsEdit, dsInsert]) then
      cdItens.Post;

    cdItens.First;

    while (not  cdItens.Eof) do
    begin

      with Frenet.Itens.New do
      begin
        Quantidade := cdItens.FieldByName('quantidade').AsInteger;
        Peso := cdItens.FieldByName('peso').AsFloat;
        Comprimento := cdItens.FieldByName('comprimento').AsFloat;
        Altura := cdItens.FieldByName('altura').AsFloat;
        Largura := cdItens.FieldByName('largura').AsFloat;
        Diametro  := cdItens.FieldByName('diametro').AsFloat;
        SKU := cdItens.FieldByName('SKU').AsString;
        Categoria := cdItens.FieldByName('Categoria').AsString;
      end;

      cdItens.Next;
    end;


    mRetFrenet.Clear;

    if not Frenet.CotarFrete then
      mRetFrenet.Lines.Add('ERRO:');

    // Ler Cotacoes
    LerRetorno;

  finally
  end;
end;

procedure TForm1.btnServicosClick(Sender: TObject);
begin

  try
    Frenet.Token := EditToken.Text;

    mRetFrenet.Clear;
    if not Frenet.ServicosDisponiveis then
      mRetFrenet.Lines.Add('ERRO:');

    // Ler Servicos retornados
    LerRetorno;

  finally
  end;

end;

procedure TForm1.FormCreate(Sender: TObject);
var
  sToken: TStringList;
  sFile: string;
begin
  try
    sToken := TStringList.Create;
    sFile := ApplicationPath + 'token.txt';


    if FileExists(sFile) then
    begin
      sToken.LoadFromFile(sFile);

      if sToken.Count >= 1 then
        EditToken.Text := sToken[0];

    end;


  finally
    sToken.Free;
  end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin


  // Criar datasets
  cdItens.FileName := ApplicationPath + 'Itens.cds';

  if not FileExists( cdItens.FileName ) then
    cdItens.CreateDataSet
  else
    cdItens.LoadFromFile;

  cdCotacao.CreateDataSet;
end;

procedure TForm1.LerRetorno;
var
  i: integer;
begin
  cdCotacao.EmptyDataSet;

  for i := 0 to Frenet.Cotacoes.Count-1 do
  begin
    cdCotacao.Append;

    cdCotacao.FieldValues['CodigoTransportador'] := Frenet.Cotacoes[i].CodigoTransportador;
    cdCotacao.FieldValues['Transportador'] := Frenet.Cotacoes[i].Transportador;
    cdCotacao.FieldValues['PrazoEntrega'] := Frenet.Cotacoes[i].PrazoEntrega;
    cdCotacao.FieldValues['Mensagem'] := Frenet.Cotacoes[i].Mensagem;
    cdCotacao.FieldValues['CodigoServico'] := Frenet.Cotacoes[i].CodigoServico;
    cdCotacao.FieldValues['DescricaoServico'] := Frenet.Cotacoes[i].DescricaoServico;
    cdCotacao.FieldValues['Valor'] := Frenet.Cotacoes[i].Valor;

    cdCotacao.FieldValues['PrazoEntregaOriginal'] := Frenet.Cotacoes[i].PrazoEntregaOriginal;
    cdCotacao.FieldValues['ValorOriginal'] := Frenet.Cotacoes[i].ValorOriginal;

    cdCotacao.FieldValues['TempoResposta'] := Frenet.Cotacoes[i].TempoResposta;



    // Erro nesta cotaçao - nao retornou valor de frete
    if Frenet.Cotacoes[i].Erro then
      cdCotacao.FieldValues['Erro'] := 'ERRO';

    cdCotacao.Post;
  end;
end;


procedure TForm1.Image1Click(Sender: TObject);
begin
  OpenURL('www.frenet.com.br');
end;

procedure TForm1.Label4Click(Sender: TObject);
begin
  OpenURL('https://painel.frenet.com.br');
end;

end.
