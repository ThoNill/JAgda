package tho.nill.JAgda;

public class AgdaName extends AgdaObject {
	private AgdaType type;
	private AgdaInstance instance;


	public AgdaName(String simpleName, boolean visible, AgdaModul modul, AgdaType type) {
		super(simpleName, visible, modul);
		this.type = type;
	}

	public AgdaInstance getInstance() {
		return instance;
	}
	
	public void setInstance(AgdaInstance instance) {
		if (this.instance == null) {
			this.instance = instance;
		} else {
			throw new AgdaException("Name ist schon mit einem Wert belegt");
		}
	}
	
	public AgdaType getType() {
		return type;
	}
	
}
