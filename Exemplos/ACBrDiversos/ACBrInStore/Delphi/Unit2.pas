unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ACBrBase, ACBrInStore, jpeg, ExtCtrls;

type
  TForm2 = class(TForm)
    edtCodigoEtiqueta: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Button1: TButton;
    edtPrefixo: TEdit;
    edtCodigo: TEdit;
    edtPeso: TEdit;
    edtTotal: TEdit;
    edtCodificacao: TEdit;
    Label6: TLabel;
    Label7: TLabel;
    edtDV: TEdit;
    ACBrInStore1: TACBrInStore;
    Image1: TImage;
    procedure Button1Click(Sender: TObject);
    procedure ACBrInStore1GetPrecoUnitario(const Codigo: string;
      var PrecoUnitario: Double);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.ACBrInStore1GetPrecoUnitario(const Codigo: string;
  var PrecoUnitario: Double);
begin
  // Faça a pesquisa do código no seu DB, e pegue o preço unitário.
  PrecoUnitario := 10.00;
end;

procedure TForm2.Button1Click(Sender: TObject);
begin
  // Define a mascara do código de barra da balança, e já acha o prefixo
  // Guardando na propriedade Prefixo.
  ACBrInStore1.Codificacao := edtCodificacao.Text;

  // Checa se o código da balança tem 13 digitos
  if Length(edtCodigoEtiqueta.Text) = 13 then
  begin
     // Verifica se o prefixo encontrado na mascara é igual ao 
     // prefixo do código de barra que veio da balança
     // Atendendo a essas situações isso quer dizer que o código recebido é
     // um código gerado pela balança.
     if ACBrInStore1.Prefixo = Copy(edtCodigoEtiqueta.Text, 1, Length(ACBrInStore1.Prefixo)) then
     begin
        ACBrInStore1.Desmembrar(edtCodigoEtiqueta.Text);

        edtPrefixo.Text := ACBrInStore1.Prefixo;
        edtCodigo.Text  := ACBrInStore1.Codigo;
        edtPeso.Text    := FloatToStr( ACBrInStore1.Peso );
        edtTotal.Text   := FloatToStr( ACBrInStore1.Total );
        edtDV.Text      := ACBrInStore1.DV;
     end;
  end;
end;

end.
