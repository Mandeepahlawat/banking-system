import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;
import java.util.concurrent.*;

public class Money extends Thread {

	private static ArrayList<HashMap<String, Object>> customers = new ArrayList<HashMap<String, Object>>();
	private static ArrayList<HashMap<String, String>> banks = new ArrayList<HashMap<String, String>>();
	private static ArrayList<String> threadList = new ArrayList<String>();
	private static BlockingQueue<HashMap<String, String>> bqMsg;
	private static int bankCount = 0;
	private String name;
	
	public Money(String name) {
		this.name = name;
	}

	public static void main(String[] args) {
		try {
			createBanks();
			createCustomers();
			bqMsg = new ArrayBlockingQueue<HashMap<String, String>>(20);
			createThreads();
		} catch (Exception ex) {
			System.out.println(ex.toString());
		}
	}
	
	public static HashMap<String, Object> getCustomer(String customerName) {
		HashMap<String, Object> customer = new HashMap<String, Object>();
		for (HashMap<String, Object> cust : customers) {
			if(cust.get("name").equals(customerName)) {
				return cust;
			}
		}
		return customer;
	}
	
	public static HashMap<String, String> getBank(String bankName) {
		HashMap<String, String> bank = new HashMap<String, String>();
		for (HashMap<String, String> b : banks) {
			if(b.get("name").equals(bankName)) {
				return b;
			}
		}
		return bank;
	}
	
	public static int getRandomAmount(String customerName) {
		HashMap<String, Object> customer = getCustomer(customerName);
		int max;
		if (Integer.parseInt(((String) customer.get("amount")).trim()) >= 50)
			max = 50;
		else
			max = Integer.parseInt((String) customer.get("amount"));
		
		Random ran = new Random();
		int amount = ran.nextInt(max) + 1;
		return amount;
	}
	
	public static String getRandomBank(String customerName) {
		HashMap<String, Object> customer = getCustomer(customerName);
		ArrayList<String> bankList =  (ArrayList<String>) customer.get("banks");
		return bankList.get(new Random().nextInt(bankList.size()));
	}
	
	public static boolean checkIfCustomerSendRequest(String customerName) {
		for(HashMap<String, String> request : bqMsg) {
			if(request.containsKey(customerName)) {
				return true;
			}
		}
		return false;
	}
	
	public static boolean checkIfCustomerSendRequestGranted(String customerName) {
		for(HashMap<String, String> request : bqMsg) {
			for(String value : request.values()) {
				if(value.contains("grant to " + customerName)) {
					return true;
				}
			}
		}
		return false;
	}
	
	public static boolean alreadyRequestedMoney(String customerName) {
		for(HashMap<String, String> request : bqMsg) {
			for(String key : request.keySet()) {
				if(key.equals(customerName)) {
					return true;
				}
			}
		}
		return false;
	}
	
	public static boolean checkIfCustomerSendRequestRejected(String customerName) {
		for(HashMap<String, String> request : bqMsg) {
			for(String value : request.values()) {
				if(value.contains("reject to " + customerName)) {
					return true;
				}
			}
		}
		return false;
	}
	
	public static boolean checkIfBankMoneyRequest(String bankName) {
		for(HashMap<String, String> request : bqMsg) {
			for(String value : request.values()) {
				if(value.contains("to " + bankName)) {
					return true;
				}
			}
		}
		return false;
	}
	
	public static boolean canGrantMoney(String bankName, int amount) {
		HashMap<String, String> bank = getBank(bankName);
		if(Integer.parseInt(bank.get("amount")) >= amount) {
			return true;
		}
		return false;
	}
	
	public static void grantMoney(String bankName, int amount, HashMap<String, String> request) {
		HashMap<String, String> bank = getBank(bankName);
		HashMap<String, String> grantRequest = new HashMap<String, String>();
		
		banks.remove(bank);
		Integer oldMoney = Integer.parseInt(bank.get("amount"));
		Integer moneyLeft = oldMoney - amount;
		bank.put("amount", moneyLeft.toString());
		banks.add(bank);
		
		grantRequest.put(bankName, "Request " + amount + " grant to " +  request.keySet().toArray()[0]);
		System.out.println(bankName + ": " + amount + " grant to " +  request.keySet().toArray()[0]);
		bqMsg.remove(request);
		bqMsg.add(grantRequest);
	}
	
	public static void rejectMoney(String bankName, int amount, HashMap<String, String> request) {
		HashMap<String, String> bank = getBank(bankName);
		HashMap<String, String> rejectRequest = new HashMap<String, String>();
		
		rejectRequest.put(bankName, "Request " + amount + " reject to " +  request.keySet().toArray()[0]);
		System.out.println(bankName + ": " + amount + " reject to " +  request.keySet().toArray()[0]);
		bqMsg.remove(request);
		bqMsg.add(rejectRequest);
	}
	
	public static boolean sendRequestForMoney(String customerName) {
		HashMap<String, String> request = new HashMap<String, String>();
		HashMap<String, Object> customer = getCustomer(customerName);
		
		if(Integer.parseInt(((String) customer.get("amount")).trim()) > 0 && !((ArrayList<String>)customer.get("banks")).isEmpty()) {
			int amount = Money.getRandomAmount(customerName);
			String bankName = Money.getRandomBank(customerName);
			request.put(customerName, " request " + amount + " to " + bankName);
			System.out.println(customerName + " request " + amount + " to " + bankName);
			bqMsg.add(request);
			return true;
		}
		else if(((ArrayList<String>)customer.get("banks")).isEmpty()) {
			System.out.println(customerName + ": no banks remaining, amount left : " + customer.get("amount"));
			return false;
		}
		else {
			System.out.println(customerName + " objective completed");
			return false;
		}
	}
	
	public static int getAmountRequested(String bankName) {
		for(HashMap<String, String> request : bqMsg) {
			for(String value : request.values()) {
				if(value.contains("to " + bankName)) {
					return Integer.parseInt(value.replaceAll("[\\D]", ""));
				}
			}
		}
		return 0;
	}
	
	public static void customerMoneyGranted(String customerName) {
		HashMap<String, Object> customer = getCustomer(customerName);
		HashMap<String, String> customerRequest = new HashMap<String, String>();
		
		for(HashMap<String, String> request : bqMsg) {
			for(String value : request.values()) {
				if(value.contains("grant to " + customerName)) {
					customerRequest = request;
					break;
				}
			}
		}
		
		Integer oldAmount = Integer.parseInt(((String) customer.get("amount")).trim());
		Integer amountGranted = Integer.parseInt(customerRequest.values().toString().replaceAll("[\\D]", "").trim());
		
		customers.remove(customer);
		customer.put("amount", Integer.toString(oldAmount - amountGranted));
		customers.add(customer);
		
		bqMsg.remove(customerRequest);
	}
	
	public static void customerMoneyRejected(String customerName) {
		HashMap<String, Object> customer = getCustomer(customerName);
		HashMap<String, String> customerRequest = new HashMap<String, String>();
		
		for(HashMap<String, String> request : bqMsg) {
			for(String value : request.values()) {
				if(value.contains("reject to " + customerName)) {
					customerRequest = request;
					break;
				}
			}
		}
		
		ArrayList<String> newBankList = new ArrayList<String>();
		
		for(String bank : (ArrayList<String>)customer.get("banks")) {
			if(!bank.equals(customerRequest.keySet().toArray()[0])) {
				newBankList.add(bank);
			}
		}
		
		customers.remove(customer);
		customer.put("banks", newBankList);
		customers.add(customer);
		
		bqMsg.remove(customerRequest);
	}
	
	public static void processAllBankRequests(String bankName) {
		for(HashMap<String, String> request : bqMsg) {
			if(request.values().toString().contains("to " + bankName)) {
				int amount = Integer.parseInt(request.values().toString().replaceAll("[\\D]", ""));
				
				if(Money.canGrantMoney(bankName, amount)){
					Money.grantMoney(bankName, amount, request);
				}
				else {
					Money.rejectMoney(bankName, amount, request);
				}
			}
		}
	}

	public void run() {
		
		ArrayList<String> customerList = new ArrayList<String>();
		
		for (HashMap<String, Object> cust : customers) {
			customerList.add((String) cust.get("name"));
		}
		
		ArrayList<String> bankList = new ArrayList<String>();
		for (HashMap<String, String> bank : banks) {
			bankList.add((String) bank.get("name"));
		}
		
		String currentThreadName = Thread.currentThread().getName();
		boolean isCustomerThread = customerList.contains(this.name);
		boolean restartThread = true;
		
		synchronized (bqMsg) {
			try {
				Thread.sleep(100);
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
				// Customer Side
			if(isCustomerThread) {
				for (int j = 0; j < threadList.size(); j++) {
					if (this.name.equals(threadList.get(j))) {
						
						// Money granted by Bank
						if(Money.checkIfCustomerSendRequestGranted(this.name)) {
							Money.customerMoneyGranted(this.name);
						}
						
						// Money rejected by Bank
						if(Money.checkIfCustomerSendRequestRejected(this.name)) {
							Money.customerMoneyRejected(this.name);
						}
						
						if(!Money.checkIfCustomerSendRequestGranted(this.name) 
								&& !Money.checkIfCustomerSendRequestRejected(this.name)
								&& !Money.alreadyRequestedMoney(this.name)) {
							restartThread = Money.sendRequestForMoney(this.name);
						}
						
					}
				}
			}
				
			// Bank Side
			if(!isCustomerThread) {
				for (int k = 0; k < threadList.size(); k++) {
					if (this.name.equals(threadList.get(k))) {
						// check if any request for current bank
						if(Money.checkIfBankMoneyRequest(this.name)) {
							Money.processAllBankRequests(this.name);
						}
					}
				}
			}
		}
	
		if(Thread.activeCount() <= Money.bankCount + 1) {
			restartThread = false;
		}
		
		if(Thread.activeCount() <= 2) {
			System.out.println("\n\n\n** Finishing **");
			for (HashMap<String, String> bank : banks) {
				System.out.println("Bank: " + bank.get("name") + " amount left: " + bank.get("amount"));
			}
		}
		
		if(restartThread) {
			Thread threadStart = new Money(this.name);
			threadStart.setName(currentThreadName);
			threadStart.start();
		}
	}

	private static void createThreads() {
		for (String str : threadList) {
			Thread threadStart = new Money(str);
			threadStart.start();
			threadStart.setName(str);
		}
	}
	
	public static void createBanks() throws IOException {
		List<String> allLines = Files
				.readAllLines(Paths.get("banks.txt"));
		System.out.println("** Banks and financial resources **\n");

		for (String line : allLines) {
			line = line.replace("{", "").replace("}", "").replace(".", "");
			Map<String, String> bankMap = new HashMap<String, String>();
			bankMap.put("name", line.split(",")[0]);
			bankMap.put("amount", line.split(",")[1]);
			banks.add((HashMap<String, String>) bankMap);
			threadList.add(line.split(",")[0]);
			System.out.println(line.split(",")[0] + " : " + line.split(",")[1]);
			bankCount++;
		}

	}
	
	public static void createCustomers() throws IOException {
		List<String> allLines = Files
				.readAllLines(Paths.get("customers.txt"));
		System.out.println("\n\n\n** Customers and loan objectives **\n");

		ArrayList<String> bankList = new ArrayList<String>();

		for (HashMap<String, String> bank : banks) {
			bankList.add((String) bank.get("name"));
		}

		for (String line : allLines) {
			line = line.replace("{", "").replace("}", "").replace(".", "");
			Map<String, Object> customerMap = new HashMap<String, Object>();
			customerMap.put("name", line.split(",")[0]);
			customerMap.put("amount", line.split(",")[1]);
			customerMap.put("banks", bankList);
			customers.add((HashMap<String, Object>) customerMap);
			threadList.add(line.split(",")[0]);
			System.out.println(line.split(",")[0] + " : " + line.split(",")[1]);
		}
		System.out.println("\n\n\n");

	}
}