using UnityEngine;

public class PlayerController : MonoBehaviour
{
    public float moveSpeed = 1f; 
    // add rotation speed for sensitivity
    public float rotationSpeed = 1f;
    public Camera playerCamera;

    private Rigidbody rb;

    void Start()
    {
        rb = GetComponent<Rigidbody>();

        Cursor.lockState = CursorLockMode.Locked;
        Cursor.visible = false;
    }

    void Update()
    {
        // Player movement
        MovePlayer();

        // Player rotation
        RotatePlayer();
    }

    void MovePlayer()
    {
        float horizontal = Input.GetAxis("Horizontal");
        float vertical = Input.GetAxis("Vertical");

        Vector3 direction = new Vector3(horizontal, 0f, vertical) * moveSpeed * Time.deltaTime;

        rb.MovePosition(transform.position + transform.TransformDirection(direction));
    }

    void RotatePlayer()
    {
        float mouseX = Input.GetAxis("Mouse X");
        float mouseY = Input.GetAxis("Mouse Y");
        float rotationX = transform.localEulerAngles.y + mouseX * rotationSpeed;

        transform.localEulerAngles = new Vector3(0, rotationX, 0);

        float rotationY = playerCamera.transform.localEulerAngles.x - mouseY * rotationSpeed;

        // clamp vertical rotation to avoid flipping over camera
        rotationY = Mathf.Clamp(rotationY, -89f, 89f);

        playerCamera.transform.localEulerAngles = new Vector3(rotationY, 0, 0);
    }
}