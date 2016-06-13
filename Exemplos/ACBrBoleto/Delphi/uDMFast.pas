unit uDMFast;

interface

uses
  System.SysUtils, System.Classes, ACBrBoleto, ACBrBoletoFCFR, ACBrBase;

type
  TdmFast = class(TDataModule)
    ACBrBoleto: TACBrBoleto;
    ACBrBoletoReport: TACBrBoletoFCFR;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dmFast: TdmFast;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

end.
